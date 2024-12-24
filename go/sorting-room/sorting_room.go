package sorting

import (
	"fmt"
	"strconv"
)

func DescribeNumber(f float64) string {
	return fmt.Sprintf("This is the number %.1f", f)
}

type NumberBox interface {
	Number() int
}

func DescribeNumberBox(nb NumberBox) string {
	number := nb.Number()
	return fmt.Sprintf("This is a box containing the number %.1f", float64(number))
}

type FancyNumber struct {
	n string
}

func (i FancyNumber) Value() string {
	return i.n
}

type FancyNumberBox interface {
	Value() string
}

func ExtractFancyNumber(fnb FancyNumberBox) int {
	if fn, ok := fnb.(FancyNumber); ok {
		if extracted, err := strconv.Atoi(fn.Value()); err == nil {
			return extracted
		}
	}
	return 0
}

func DescribeFancyNumberBox(fnb FancyNumberBox) string {
	extracted := ExtractFancyNumber(fnb)
	return fmt.Sprintf("This is a fancy box containing the number %.1f", float64(extracted))
}

func DescribeAnything(i interface{}) string {
	switch v := i.(type) {
	case float64:
		return DescribeNumber(v)
	case int:
		return DescribeNumber(float64(v))
	case NumberBox:
		return DescribeNumberBox(v)
	case FancyNumberBox:
		return DescribeFancyNumberBox(v)
	default:
		return "Return to sender"
	}
}
