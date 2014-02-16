#!/usr/bin/env python

class Student:
    def __init__(self, firstname, lastname, finalexamscore):
        self.firstname      = firstname
        self.lastname       = lastname
        self.finalexamscore = finalexamscore

    def has_passed(self):
        return self.finalexamscore >= 60

def passed(students):
    passed_students = []
    for student in students:
        if student.has_passed():
            passed_students.append(student)
    return passed_students

students = [Student("John", "Deer", 60),
            Student("Billy", "Bob", 49.1),
            Student("Jane", "Doe", 89),
            Student("Jack", "Johnson", 29.3)]

if __name__ == '__main__':
    print  "Students that have passed:\n", passed(students)
