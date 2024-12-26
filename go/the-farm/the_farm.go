package thefarm

import (
	"errors"
	"fmt"
)

type InvalidCowsError struct {
	n       int
	message string
}

func (e *InvalidCowsError) Error() string {
	return fmt.Sprintf("%d cows are invalid: %s", e.n, e.message)
}

func DivideFood(calc FodderCalculator, n int) (float64, error) {
	fa, err := calc.FodderAmount(n)
	if err != nil {
		return 0.0, err
	}
	ff, err := calc.FatteningFactor()
	if err != nil {
		return 0.0, err
	}
	return fa * ff / float64(n), nil
}

func ValidateInputAndDivideFood(calc FodderCalculator, n int) (float64, error) {
	if n > 0 {
		return DivideFood(calc, n)
	} else {
		return 0.0, errors.New("invalid number of cows")
	}
}

func ValidateNumberOfCows(n int) error {
	if n < 0 {
		return &InvalidCowsError{n, "there are no negative cows"}
	}
	if n == 0 {
		return &InvalidCowsError{n, "no cows don't need food"}
	}
	return nil
}
