// COP4020 JavaScript Assignment 1 - Grant Hendrickson
/*
  Problem 1:

  The following iterative sequence is defined for the set of positive integers:
  n -> n/2    (n is even)
  n -> 3n + 1 (n is odd)

  Which starting number, under one million, produces the longest chain?
*/

function collatzSequenceLength(n) {
	let length = 1;
	while (n !== 1) {
		if (n % 2 === 0) {
			n /= 2;
		} else {
			n = 3 * n + 1;
		}
		length++;
	}
	return length;
}

function findLongestCollatzSequence(limit) {
	let longestChain = 0;
	let startingNumber = 0;

	for (let i = 2; i < limit; i++) {
		const chainLength = collatzSequenceLength(i);
		if (chainLength > longestChain) {
			longestChain = chainLength;
			startingNumber = i;
		}
	}

	return startingNumber;
}

const limit = 1000000;
const result = findLongestCollatzSequence(limit);

console.log(
	`For problem 1 the starting number under ${limit} that produces the longest chain is: ${result}`
);

//  Problem 2:
//  Given n find the sum of the digits in n!

const bigInt = require('big-integer');

function factorial(n) {
	if (n === 0 || n === 1) {
		return bigInt(1);
	} else {
		return bigInt(n).times(factorial(n - 1));
	}
}

function sumOfDigitsInFactorial(n) {
	const factorialResult = factorial(n).toString();
	let sum = 0;

	for (let i = 0; i < factorialResult.length; i++) {
		sum += parseInt(factorialResult.charAt(i));
	}

	return sum;
}

// Example usage:
const n = 100; // Change this to your desired value of n
const sumOfDigits = sumOfDigitsInFactorial(n);

console.log(`The sum of the digits in ${n}! is: ${sumOfDigits}`);
