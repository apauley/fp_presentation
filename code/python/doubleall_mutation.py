#! /usr/bin/env python

def doubleAll(numbers):
    count = 0
    for num in numbers:
        numbers[count] = num * 2
        count += 1

if __name__ == '__main__':
    nums1 = [5,3,1]
    nums2 = nums1
    print doubleAll(nums1)
    print nums1
    print nums2
