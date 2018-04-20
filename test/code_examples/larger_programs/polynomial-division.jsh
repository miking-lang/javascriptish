function pop(arr) {
	var n = arr.length
	var i = 0
	var newarr = []
	while ( i < n - 1) {
		newarr[i] = arr[i]
		i = i + 1
	}
	return newarr
}

function copy(arr) {
	var n = arr.length 
	var i = 0
	var newarr = []
	while ( i < n ) {
		newarr[i] = arr[i]
		i = i + 1
	}
	return newarr
}

function degree(poly) {
	while ( poly[poly.length-1] == 0) {
		poly = pop(poly)
	}
	return poly.length - 1
}

function zeros(num) {
	var i = 0
	var arr = []
	while ( i < num ) {
		arr[i] = 0
		i = i + 1
	}
	return arr
}

function polyShiftRight(p, places) {
	if ( places <= 0 ) {
		return p
	}
	var pd = degree(p)
	var d = copy(p)
	var i = pd 
	while ( i >= 0 ) {
		d[i+places] = d[i]
		d[i] = 0
		i = i - 1
	}
	return d
}

function polyMultiply(p, m) {
	var i = 0
	var n = p.length 
	while ( i < n ) {
		p[i] = p[i] * m
		i = i + 1
	}
}

function polySubtract(p, s) {
	var i = 0
	var n = p.length 
	while ( i < n ) {
		p[i] = p[i] - s[i]
		i = i + 1
	}
}

function poly_print(poly) {
	var pd = degree(poly)
	var i = pd 
	var result = ""
	while ( i >= 0) {
		var coeff = poly[i]
		if ( coeff != 0 ) {
			if ( coeff == 1 ) {
				if ( i < pd ) {
					result = result + " + "
				}
			} else {
				if ( coeff == -1 ) {
					if ( i < pd ) {
						result = result + " - "
					} else {
						result = result + "-"
					}
				} else {
					if ( coeff < 0 ) {
						if ( i < pd ) {
							result = result + " - " + -coeff
						} else {
							result = result + coeff
						}
					} else {
						if ( i < pd ) {
							result = result + " + " + -coeff
						} else {
							result = result + coeff
						}
					}
				}
			}
			if ( i > 1 ) {
				result = result + "x^" + i 
			} else {
				if ( i == 1 ) {
					result = result + "x"
				}
			}
		}
		i = i - 1
	}
	console.log(result)
}

function poly_div(N, D) {
	console.log("Calc...")
	var dd = degree(D)
	var nd = degree(N)
	if ( dd < 0 ) {
		console.log("DIVISION BY ZERO")
		return -1
	}
	n2 = copy(N)
	var q = zeros(nd)
	console.log(nd)
	console.log(dd)
	while ( nd >= dd ) {
		console.log("Yeah")
		var d2 = polyShiftRight(D, nd-dd)
		q[nd - dd] = n2[nd] / d2[nd]
		polyMultiply(d2, q[nd-dd])
		polySubtract(n2, d2)
		nd = degree(n2)
	}
	console.log("Quotient:")
	poly_print(q)
	console.log("Reminder:")
	poly_print(n2)
}

function main() {
	var n = [-42, 0, -12, 1]
	var d = [-3, 1, 0, 0]
	console.log("Numerator: ")
	poly_print(n)
	console.log("Denominator: ")
	poly_print(d)
	poly_div(n, d)
}
main()

