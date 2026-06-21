pub fn reply<'a>(message: &'a str) -> &'a str {
    let msg = message.trim();
    let question = msg.ends_with('?');
    let shouting = msg.chars().any(|c| c.is_alphabetic()) && msg == msg.to_uppercase();

    match () {
        _ if msg.is_empty() => "Fine. Be that way!",
        _ if shouting && question => "Calm down, I know what I'm doing!",
        _ if shouting => "Whoa, chill out!",
        _ if question => "Sure.",
        _ => "Whatever.",
    }
}
