	.text

	li	$v0, 5	#read integer code
	syscall
	
	
	div	$a0, $v0, 7	# divide by 7
	
	
	li	$v0, 1	#print integer code
	syscall
	
	
	li	$v0, 1	#end the program
	syscall
	
