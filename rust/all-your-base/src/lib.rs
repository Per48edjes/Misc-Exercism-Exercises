use num_traits::PrimInt;

#[derive(Debug, PartialEq, Eq)]
pub enum Error<T> {
    InvalidInputBase,
    InvalidOutputBase,
    InvalidDigit(T),
    Overflow,
}

pub fn convert<T: PrimInt>(number: &[T], from_base: T, to_base: T) -> Result<Vec<T>, Error<T>> {
    let one = T::one();
    match (from_base, to_base) {
        _ if from_base <= one => Err(Error::InvalidInputBase),
        _ if to_base <= one => Err(Error::InvalidOutputBase),
        _ => {
            // convert into common base 10
            let mut value = T::zero();
            for &digit in number {
                if digit >= from_base {
                    return Err(Error::InvalidDigit(digit));
                }
                value = value
                    .checked_mul(&from_base)
                    .and_then(|v| v.checked_add(&digit))
                    .ok_or(Error::Overflow)?;
            }

            // convert out of common base 10
            let mut digits = Vec::new();
            let zero = T::zero();
            while value > zero {
                digits.push(value % to_base);
                value = value / to_base;
            }
            if digits.is_empty() {
                digits.push(zero);
            }
            digits.reverse();
            Ok(digits)
        }
    }
}
