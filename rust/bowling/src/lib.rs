use frames::Frames;

#[derive(Debug, PartialEq, Eq)]
pub enum Error {
    NotEnoughPinsLeft,
    GameComplete,
}

pub struct BowlingGame {
    frames: Frames,
}

impl BowlingGame {
    pub fn new() -> Self {
        BowlingGame {
            frames: Frames::new(),
        }
    }

    pub fn roll(&mut self, pins: u16) -> Result<(), Error> {
        if self.frames.is_complete() {
            return Err(Error::GameComplete);
        }
        if pins > self.frames.pins_left_in_frame() {
            return Err(Error::NotEnoughPinsLeft);
        }
        self.frames.record(pins);
        Ok(())
    }

    pub fn score(&self) -> Option<u16> {
        self.frames.is_complete().then(|| self.frames.total())
    }
}

mod frames {
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    enum StandardFrame {
        Empty,
        InProgress { first: u16 },
        Open { first: u16, second: u16 },
        Spare { first: u16 },
        Strike,
    }

    impl StandardFrame {
        fn pins_left(&self) -> u16 {
            match self {
                StandardFrame::Empty => 10,
                StandardFrame::InProgress { first } => 10 - first,
                StandardFrame::Open { .. }
                | StandardFrame::Spare { .. }
                | StandardFrame::Strike => {
                    unreachable!("pins_left called on a completed standard frame")
                }
            }
        }

        fn record(&mut self, pins: u16) -> bool {
            match *self {
                StandardFrame::Empty if pins == 10 => {
                    *self = StandardFrame::Strike;
                    true
                }
                StandardFrame::Empty => {
                    *self = StandardFrame::InProgress { first: pins };
                    false
                }
                StandardFrame::InProgress { first } if first + pins == 10 => {
                    *self = StandardFrame::Spare { first };
                    true
                }
                StandardFrame::InProgress { first } => {
                    *self = StandardFrame::Open {
                        first,
                        second: pins,
                    };
                    true
                }
                StandardFrame::Open { .. }
                | StandardFrame::Spare { .. }
                | StandardFrame::Strike => {
                    unreachable!("record called on a completed standard frame")
                }
            }
        }

        fn points(&self) -> u16 {
            match self {
                StandardFrame::Empty => 0,
                StandardFrame::InProgress { first } => *first,
                StandardFrame::Open { first, second } => first + second,
                StandardFrame::Spare { .. } | StandardFrame::Strike => 10,
            }
        }
    }

    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    enum TenthFrame {
        Empty,
        FirstRoll { base: u16 },
        OpenDone { base: [u16; 2] },
        SpareAwaitingFill { base: [u16; 2] },
        StrikeAwaitingTwoFills { base: u16 },
        StrikeAwaitingOneFill { base: u16, fill: u16 },
        SpareDone { base: [u16; 2], fill: u16 },
        StrikeDone { base: u16, fills: [u16; 2] },
    }

    impl TenthFrame {
        fn pins_left(&self) -> u16 {
            match self {
                TenthFrame::Empty => 10,
                TenthFrame::FirstRoll { base } => 10 - base,
                TenthFrame::SpareAwaitingFill { .. }
                | TenthFrame::StrikeAwaitingTwoFills { .. } => 10,
                TenthFrame::StrikeAwaitingOneFill { fill, .. } => {
                    if *fill == 10 {
                        10
                    } else {
                        10 - fill
                    }
                }
                TenthFrame::OpenDone { .. }
                | TenthFrame::SpareDone { .. }
                | TenthFrame::StrikeDone { .. } => {
                    unreachable!("pins_left called on a completed tenth frame")
                }
            }
        }

        fn record(&mut self, pins: u16) -> bool {
            *self = match *self {
                TenthFrame::Empty if pins == 10 => TenthFrame::StrikeAwaitingTwoFills { base: 10 },
                TenthFrame::Empty => TenthFrame::FirstRoll { base: pins },
                TenthFrame::FirstRoll { base } if base + pins == 10 => {
                    TenthFrame::SpareAwaitingFill { base: [base, pins] }
                }
                TenthFrame::FirstRoll { base } => TenthFrame::OpenDone { base: [base, pins] },
                TenthFrame::SpareAwaitingFill { base } => {
                    TenthFrame::SpareDone { base, fill: pins }
                }
                TenthFrame::StrikeAwaitingTwoFills { base } => {
                    TenthFrame::StrikeAwaitingOneFill { base, fill: pins }
                }
                TenthFrame::StrikeAwaitingOneFill { base, fill } => TenthFrame::StrikeDone {
                    base,
                    fills: [fill, pins],
                },
                TenthFrame::OpenDone { .. }
                | TenthFrame::SpareDone { .. }
                | TenthFrame::StrikeDone { .. } => {
                    unreachable!("record called on a completed tenth frame")
                }
            };
            self.is_complete()
        }

        fn is_complete(&self) -> bool {
            matches!(
                self,
                TenthFrame::OpenDone { .. }
                    | TenthFrame::SpareDone { .. }
                    | TenthFrame::StrikeDone { .. }
            )
        }

        fn points(&self) -> u16 {
            match self {
                TenthFrame::Empty => 0,
                TenthFrame::FirstRoll { base } => *base,
                TenthFrame::SpareAwaitingFill { base } | TenthFrame::OpenDone { base } => {
                    base[0] + base[1]
                }
                TenthFrame::StrikeAwaitingTwoFills { base } => *base,
                TenthFrame::StrikeAwaitingOneFill { base, fill } => base + fill,
                TenthFrame::SpareDone { base, fill } => base[0] + base[1] + fill,
                TenthFrame::StrikeDone { base, fills } => base + fills[0] + fills[1],
            }
        }
    }

    pub(crate) struct Frames {
        standard: [StandardFrame; 9],
        tenth: TenthFrame,
        cursor: usize,
    }

    impl Frames {
        pub(crate) fn new() -> Self {
            Frames {
                standard: [StandardFrame::Empty; 9],
                tenth: TenthFrame::Empty,
                cursor: 0,
            }
        }

        pub(crate) fn is_complete(&self) -> bool {
            self.cursor >= 10
        }

        pub(crate) fn total(&self) -> u16 {
            let mut total = self.tenth.points();

            for (i, frame) in self.standard.iter().enumerate() {
                total += frame.points();
                total += self.bonus_after(i, frame);
            }

            total
        }

        fn bonus_after(&self, frame_index: usize, frame: &StandardFrame) -> u16 {
            let count = match frame {
                StandardFrame::Spare { .. } => 1,
                StandardFrame::Strike => 2,
                _ => return 0,
            };

            let mut remaining = count;
            let mut sum = 0;
            let mut index = frame_index + 1;

            while remaining > 0 && index < 9 {
                match self.standard[index] {
                    StandardFrame::Strike => {
                        sum += 10;
                        remaining -= 1;
                        index += 1;
                    }
                    StandardFrame::Spare { first } => {
                        sum += first;
                        remaining -= 1;
                        if remaining > 0 {
                            sum += 10 - first;
                            remaining -= 1;
                        }
                        index += 1;
                    }
                    StandardFrame::Open { first, second } => {
                        sum += first;
                        remaining -= 1;
                        if remaining > 0 {
                            sum += second;
                            remaining -= 1;
                        }
                        index += 1;
                    }
                    StandardFrame::Empty | StandardFrame::InProgress { .. } => break,
                }
            }

            if remaining > 0 {
                let leading = match self.tenth {
                    TenthFrame::Empty => vec![],
                    TenthFrame::FirstRoll { base } => vec![base],
                    TenthFrame::SpareAwaitingFill { base } | TenthFrame::OpenDone { base } => {
                        vec![base[0], base[1]]
                    }
                    TenthFrame::StrikeAwaitingTwoFills { base } => vec![base],
                    TenthFrame::StrikeAwaitingOneFill { base, fill } => vec![base, fill],
                    TenthFrame::SpareDone { base, fill } => vec![base[0], base[1], fill],
                    TenthFrame::StrikeDone { base, fills } => vec![base, fills[0], fills[1]],
                };
                sum += leading.into_iter().take(remaining).sum::<u16>();
            }

            sum
        }

        pub(crate) fn pins_left_in_frame(&self) -> u16 {
            if self.cursor < 9 {
                self.standard[self.cursor].pins_left()
            } else {
                self.tenth.pins_left()
            }
        }

        pub(crate) fn record(&mut self, pins: u16) {
            let completed = if self.cursor < 9 {
                self.standard[self.cursor].record(pins)
            } else {
                self.tenth.record(pins)
            };
            if completed {
                self.cursor += 1;
            }
        }
    }
}
