/*
	A program that uses bubble sort to sort one array of int and one array of string
 */

function bubbleSort(items) {
	var n = items.length
	var swapped = true
	while (swapped) {
		swapped = false
		var i = 1
		while (i < n) {
			if ( items[i-1] > items[i] ) {
				var tmp = items[i]
				items[i] = items[i-1]
				items[i-1] = tmp
				swapped = true
			}
			i = i + 1
		}
	}
	return items
}

function main() {
	var arr1 = [9,1,3,2,5,1,2,6,2]
	var arr2 = ["friday","cloud","morning","brownish","descends","every","A"]
	arr1 = bubbleSort(arr1)
	arr2 = bubbleSort(arr2)
	console.log(arr1)
	console.log(arr2)
}

main()



