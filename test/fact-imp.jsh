

function fact(n){
  var res=1
  while(n>1){
    res = res * n
    n = n - 1
  }
  return res
}

print(fact(5))