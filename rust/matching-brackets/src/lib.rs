pub fn brackets_are_balanced(string: &str) -> bool {
    let mut stack: Vec<char> = Vec::new();
    for c in string.chars() {
        if matches!(c, '(' | '[' | '{') {
            stack.push(c);
            continue;
        };
        if matches!(c, ')' | ']' | '}') {
            match (stack.last(), c) {
                (Some(&'('), ')') | (Some(&'['), ']') | (Some(&'{'), '}') => {
                    stack.pop();
                }
                _ => return false,
            }
        }
    }
    stack.is_empty()
}
