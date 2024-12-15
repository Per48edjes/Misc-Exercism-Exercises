package interest

// InterestRate returns the interest rate for the provided balance.
func InterestRate(balance float64) float32 {
	switch {
	case balance < 0:
		return 3.213
	case balance < 1000:
		return 0.5
	case balance < 5000:
		return 1.621
	default:
		return 2.475
	}
}

// Interest calculates the interest for the provided balance.
func Interest(balance float64) float64 {
	return balance * float64(InterestRate(balance)/100.0)
}

// AnnualBalanceUpdate calculates the annual balance update, taking into account the interest rate.
func AnnualBalanceUpdate(balance float64) float64 {
	return balance + Interest(balance)
}

// GreaterThan checks if a is greater than b for any ordered type.
func GreaterThan(a float64, b float64) bool {
	return a > b
}

// LessThan checks if a is less than b for any ordered type.
func LessThan(a float64, b float64) bool {
	return a < b
}

// YearsBeforeDesiredBalance calculates the minimum number of years required to reach the desired balance.
func YearsBeforeDesiredBalance(balance, targetBalance float64) int {
	if (balance < 0 && targetBalance > 0) || (balance > 0 && targetBalance < 0) {
		panic("Balance of different signs never converge!")
	}

	var compFn func(float64, float64) bool
	if balance > 0 && targetBalance > 0 {
		compFn = LessThan
	} else {
		compFn = GreaterThan
	}
	years := 0
	for ; compFn(balance, targetBalance); years++ {
		balance = AnnualBalanceUpdate(balance)
	}
	return years
}
