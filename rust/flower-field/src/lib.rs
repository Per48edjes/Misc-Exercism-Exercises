pub fn annotate(garden: &[&str]) -> Vec<String> {
    let mut counts: Vec<Vec<char>> = Vec::new();

    // first pass to populate flowers
    for (i, row) in garden.iter().enumerate() {
        counts.push(vec!['_'; row.len()]);
        for (j, cell) in row.chars().enumerate() {
            counts[i][j] = cell;
        }
    }

    // Cartesion product of dimensions
    let xs = 0..garden.len();
    let coords = xs.flat_map(|x| (0..garden[x].len()).map(move |y| (x, y)));

    let count_surrounding = |(x, y): (usize, usize)| {
        if counts[x][y] == '*' {
            return;
        }
        static DIRS: [(isize, isize); 8] = [
            (-1, -1),
            (-1, 0),
            (-1, 1),
            (0, -1),
            (0, 1),
            (1, -1),
            (1, 0),
            (1, 1),
        ];
        let mut acc: u32 = 0;
        for (dx, dy) in DIRS {
            let spot: Option<&char> = x
                .checked_add_signed(dx)
                .zip(y.checked_add_signed(dy))
                .and_then(|(x_prime, y_prime)| counts.get(x_prime)?.get(y_prime));
            if spot == Some(&'*') {
                acc += 1;
            }
        }
        if acc > 0 {
            counts[x][y] = char::from_digit(acc, 10).unwrap();
        }
    };

    coords.for_each(count_surrounding);

    counts
        .iter()
        .map(|inner| String::from_iter(inner.iter()))
        .collect()
}
