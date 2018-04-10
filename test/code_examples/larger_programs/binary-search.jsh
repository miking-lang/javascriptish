/*
	A program that uses binary search to find elements in an array of int
 */

function floor(num) {
	return num - (num % 1)
}

function binarySearch(items, value) {
	var startIndex  = 0
	var stopIndex   = items.length - 1
	var middle      = floor((stopIndex + startIndex)/2)
		
	while(items[middle] != value && startIndex < stopIndex){
	
		//adjust search area
		if (value < items[middle]){
			stopIndex = middle - 1       
		} else if (value > items[middle]){
			startIndex = middle + 1
		}
		
		//recalculate middle
		middle = floor((stopIndex + startIndex)/2)   
	}

	//make sure it's the right value
	if (items[middle] != value) {
		return -1
	} else {
		return middle
	}
}

// Test cases
var arr = [7,2,3,5,8,4,2,5,1]
if ( binarySearch(arr,8) == 4 ) {
	console.log("Number 8 was found correctly")
} else {
	console.log("We could not find number 8 a place 4")
}

if ( binarySearch(arr, 57) == -1 ) {
	console.log("Number 57 does not exists in array, that is good")
} else {
	console.log("We were able to find number 57 in some strange way")
}