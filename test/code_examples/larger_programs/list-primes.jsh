// Program that lists all primes up to number n

function is_prime(number) {
if (number <= 1 ) {
return false
}
var i = 2
var zero = 0
while ( i*i < number) {
var reminder = number % i
if ( reminder == zero ) {
	return false
}
i += 1
}
return true
}

function find_primes(limit) {
var i = 2;
while ( i < limit ) {
if ( is_prime(i) ) {
	print(i)
}
i += 1
}
}
find_primes(1000)