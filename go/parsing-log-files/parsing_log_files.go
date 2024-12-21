package parsinglogfiles

import (
	"fmt"
	"regexp"
)

func IsValidLine(text string) bool {
	re := regexp.MustCompile(`^(\[TRC\]|\[DBG\]|\[INF\]|\[WARN\]|\[ERR\]|\[FTL\]){1}\s+`)
	return re.MatchString(text)
}

func SplitLogLine(text string) []string {
	re := regexp.MustCompile(`<[\~\*\-\=]*>`)
	return re.Split(text, -1)
}

func CountQuotedPasswords(lines []string) int {
	re, count := regexp.MustCompile(`\".*(?i)password.*\"`), 0
	for _, line := range lines {
		if re.MatchString(line) {
			count++
		}
	}
	return count
}

func RemoveEndOfLineText(text string) string {
	re := regexp.MustCompile(`end\-of\-line\d+`)
	return re.ReplaceAllString(text, "")
}

func TagWithUserName(lines []string) []string {
	re := regexp.MustCompile(`User\s+([^\s]+)\s+`)
	var results []string
	for _, line := range lines {
		match := re.FindStringSubmatch(line)
		if match != nil {
			results = append(results, fmt.Sprintf("[USR] %s ", match[1])+line)
		} else {
			results = append(results, line)
		}
	}
	return results
}
