package chessboard

type (
	File       []bool
	Chessboard map[string]File
)

// CountInFile returns how many squares are occupied in the chessboard,
// within the given file.
func CountInFile(cb Chessboard, file string) int {
	squares, ok := cb[file]
	if !ok {
		return 0
	}
	count := 0
	for _, occupied := range squares {
		if occupied {
			count++
		}
	}
	return count
}

// CountInRank returns how many squares are occupied in the chessboard,
// within the given rank.
func CountInRank(cb Chessboard, rank int) int {
	if rank < 1 || rank > 8 {
		return 0
	}
	count := 0
	for _, squares := range cb {
		if squares[rank-1] {
			count++
		}
	}
	return count
}

// CountAll should count how many squares are present in the chessboard.
func CountAll(cb Chessboard) int {
	squares := 0
	for _, file := range cb {
		squares += len(file)
	}
	return squares
}

// CountOccupied returns how many squares are occupied in the chessboard.
func CountOccupied(cb Chessboard) int {
	squares := 0
	for file := range cb {
		squares += CountInFile(cb, file)
	}
	return squares
}
