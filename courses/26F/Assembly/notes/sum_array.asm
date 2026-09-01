	.data
size:	.word 6
array:	.word 1, 2, 3, 4, 5, 6, 7

	.text
	li	$a0, 0		# accumulator a0 = 0
	lw	$t0, size	# array size (in words)
	mul	$t0, $t0, 4	# array size (in bytes)
	li	$t1, 0		# index t1 = 0
	
sum:	bge	$t1, $t0, end	# end when i >= size
	lw	$t2, array($t1)	# t2 = array[t1]
	add	$a0, $a0, $t2	# a0 = a0 + t2
	add	$t1, $t1, 4	# t1++
	j	sum

end:	li	$v0, 1		# print the sum in a0
	syscall
	li	$v0, 10		# end the program
	syscall
