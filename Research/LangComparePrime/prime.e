def divisibleRec(i, var j) {
  while(j>0) {
    if (j==1) {
	return false
    }
    if (i%j==0) {
    	 return true
    }
    j := j - 1
  }
}


def divisible(i) {
  return divisibleRec(i, i-1)
}

def main(){
  var count := 0;
  for i in 2..20000 {
    if(!divisible(i)){
      count := count+1
    }
  }
  println(count)
}

main()