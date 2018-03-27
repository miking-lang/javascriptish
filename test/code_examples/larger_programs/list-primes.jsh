// Program that lists all primes up to number n

function is_prime(number) {
	if (number <= 1 ) {
		return false
	}
	var i = 2
	while ( i*i < number) {
		if ( number % i == 0 ) {
			return false
		}
		i = i + 1
	}
	return true
}

function find_primes(limit) {
	var i = 2
	while ( i < limit ) {
		if ( is_prime(i) ) {
			print(i)
		}
	i = i + 1
	}
}
find_primes(1000)