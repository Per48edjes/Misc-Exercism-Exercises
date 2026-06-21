use std::fmt::Write;

pub fn build_proverb(list: &[&str]) -> String {
    let Some(first) = list.first() else {
        return String::new();
    };

    let mut proverb = String::new();
    for window in list.windows(2) {
        let _ = writeln!(
            proverb,
            "For want of a {} the {} was lost.",
            window[0], window[1]
        );
    }
    let _ = write!(proverb, "And all for the want of a {first}.");

    proverb
}
