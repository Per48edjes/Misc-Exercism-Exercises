#include "grade_school.h"
#include <string.h>

void init_roster(roster_t *roster)
{
    roster->count = 0;
}

roster_t get_grade(const roster_t *const roster, int desired_grade)
{
    roster_t grade_roster = { .count = 0 };
    for (size_t i = 0; i < roster->count; ++i)
    {
        if (roster->students[i].grade == desired_grade)
        {
            grade_roster.students[grade_roster.count++] = roster->students[i];
        }
    }
    return grade_roster;
}

static int new_student_pos(const roster_t *const roster, char *name, int grade)
{
    size_t idx = roster->count;
    for (size_t i = 0; i < roster->count; ++i)
    {
        int name_match = strcmp(name, roster->students[i].name);
        if (name_match == 0)
        {
            return -1;
        }

        if ((grade < roster->students[i].grade) ||
            (grade == roster->students[i].grade && name_match < 0))
        {
            idx = (idx < i) ? idx : i;
        }
    }

    return (int)idx;
}

bool add_student(roster_t *roster, char *name, int grade)
{
    if (roster->count < MAX_STUDENTS)
    {
        int idx = new_student_pos(roster, name, grade);
        if (idx >= 0)
        {
            if (strlen(name) + 1 > MAX_NAME_LENGTH)
            {
                return false;
            }
            memmove(&roster->students[idx + 1], &roster->students[idx],
                    (roster->count - idx) * sizeof(student_t));
            roster->students[idx].grade = grade;
            strncpy(roster->students[idx].name, name, MAX_NAME_LENGTH);
            roster->count++;
            return true;
        }
    }

    return false;
}
