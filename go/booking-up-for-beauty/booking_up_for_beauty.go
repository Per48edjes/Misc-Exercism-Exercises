package booking

import "time"

// Schedule returns a time.Time from a string containing a date.
func Schedule(date string) time.Time {
	layout := "1/_2/2006 15:04:05"
	if t, err := time.Parse(layout, date); err != nil {
		panic(err)
	} else {
		return t
	}
}

// HasPassed returns whether a date has passed.
func HasPassed(date string) bool {
	layout := "January _2, 2006 15:04:05"
	if t, err := time.Parse(layout, date); err != nil {
		panic(err)
	} else {
		return time.Now().After(t)
	}
}

// IsAfternoonAppointment returns whether a time is in the afternoon.
func IsAfternoonAppointment(date string) bool {
	layout := "Monday, January _2, 2006 15:04:05"
	if t, err := time.Parse(layout, date); err != nil {
		panic(err)
	} else {
		return t.Hour() >= 12 && t.Hour() < 18
	}
}

// Description returns a formatted string of the appointment time.
func Description(date string) string {
	layout := "1/_2/2006 15:04:05"
	if t, err := time.Parse(layout, date); err != nil {
		panic(err)
	} else {
		outputLayout := "Monday, January 2, 2006, at 15:04"
		return "You have an appointment on " + t.Format(outputLayout) + "."
	}
}

// AnniversaryDate returns a Time with this year's anniversary.
func AnniversaryDate() time.Time {
	return time.Date(time.Now().Year(), time.September, 15, 0, 0, 0, 0, time.UTC)
}
