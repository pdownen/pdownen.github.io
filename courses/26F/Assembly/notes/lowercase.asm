	.data
msg:	.ascii	"SAY IT A LITTLE QUIETER\0"


	.text
	li	$t0, 'a'
	xor	$t0, 'A'	#difference between a, A
	li	$t1, 0		#index i into msg

loop:	lb	$t2, msg($t1)	#get next char msg[i]
	beqz	$t2, end	#stop at null char
	blt	$t2, 'A', next	#only change chars
	bgt	$t2, 'Z', next	#between 'A' and 'Z'
	or	$t2, $t2, $t0	#set case bit to 1 for lowercase
	sb	$t2, msg($t1)	#replace msg[i]

next:	add	$t1, $t1, 1	#next char i++
	j	loop		#repeat loop

end:	li	$v0, 4		#print a string
	la	$a0, msg	#pass msg address
	syscall			#call print
	li	$v0, 10		#exit program
	syscall
