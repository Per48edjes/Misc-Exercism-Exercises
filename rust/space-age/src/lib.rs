// The code below is a stub. Just enough to satisfy the compiler.
// In order to pass the tests you can add-to or change any of this code.

#[derive(Debug)]
pub struct Duration {
    seconds: u64,
}

impl From<u64> for Duration {
    fn from(s: u64) -> Self {
        Duration { seconds: s }
    }
}

pub trait Planet {
    const ORBITAL_PERIOD: f64;
    const EARTH_YEAR_SECONDS: u64 = 31_557_600;
    fn years_during(d: &Duration) -> f64 {
        (d.seconds as f64 / Self::EARTH_YEAR_SECONDS as f64) / Self::ORBITAL_PERIOD
    }
}

macro_rules! impl_planets {
    ( $( ($planet:ident, $orbital_period:expr) ),*$(,)? ) => {
        $(
            pub struct $planet {}

            impl Planet for $planet {
                const ORBITAL_PERIOD: f64 = $orbital_period;
            }
        )*
    };
}

impl_planets!(
    (Mercury, 0.2408467),
    (Venus, 0.61519726),
    (Earth, 1.0),
    (Mars, 1.8808158),
    (Jupiter, 11.862615),
    (Saturn, 29.447498),
    (Uranus, 84.016846),
    (Neptune, 164.179132),
);
