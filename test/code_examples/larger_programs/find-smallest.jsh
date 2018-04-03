// Find the smallest number that is divisible by the numbers 1 to limit
// https://projecteuler.net/problem=5

function is_divisible_up_to(number, limit) {
	var i = 2
	while ( i <= limit ) {
		if ( number % i != 0 ) {
			return false
		}
		i = i + 1
	}
	return true
}

function find_smallest_number(limit) {
	var number = limit*limit
	while (true) {
		if ( is_divisible_up_to(number, limit) ) {
			return number
		}
		number = number + 1
	}
}

print(find_smallest_number(20))