	.text
main:	li 	$v0, 5		# Code to get a number
	syscall			# User's n stored in $v0
	
	li 	$a0, 1		# Initial value a = 1

loop:	blez	$v0, end	# Loop ends at n <= 0
	mul	$a0, $a0, $v0	# a = a * n
	sub	$v0, $v0, 1	# n--
	j 	loop		# Restart loop

end:	li 	$v0, 1		# Code to print number
	syscall
	li	$v0, 17		# Code to exit
	syscall
