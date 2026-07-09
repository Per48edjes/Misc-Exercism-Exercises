pub struct Luhn {
    code: String,
}

impl Luhn {
    pub fn is_valid(&self) -> bool {
        match Self::parse_number(&self.code) {
            Some(nums) if nums.len() > 1 => Self::validate_number(nums),
            _ => false,
        }
    }

    fn parse_number(code: &str) -> Option<Vec<u32>> {
        code.chars()
            .filter(|c| !c.is_whitespace())
            .map(|c| c.to_digit(10))
            .collect()
    }

    fn validate_number(mut nums: Vec<u32>) -> bool {
        nums.iter_mut()
            .rev()
            .enumerate()
            .map(|(i, v)| {
                if i % 2 == 1 {
                    *v *= 2;
                    if *v > 9 {
                        *v -= 9;
                    }
                }
            })
            .for_each(drop);

        nums.into_iter().sum::<u32>() % 10 == 0
    }
}

impl<T> From<T> for Luhn
where
    T: ToString,
{
    fn from(input: T) -> Self {
        Luhn {
            code: input.to_string(),
        }
    }
}
