	.data
t:	.word 10, tl, tr
tl:	.word 20, tll, tlr
tll:	.word 40, 0, 0
tlr:	.word 30, 0, 0
tr:	.word 6, trl, trr
trl:	.word 7, 0, 0
trr:	.word 8, 0, 0





	.text
main:
	la   $a0, t
	jal  sum_tree
	move $a0, $v0
	jal  print_int
	jal  exit

# Sum up the labels in the nodes of a tree,
#   positive to the left
#   negative to the right
# Arguments:
#   $a0 = the tree node, which is either:
#           * a pointer to 3 words in memory holding
#               1. The numeric label of the node
#               2. The pointer to the left sub-tree
#               3. The pointer to the right sub-tree
#           * an empty leaf (null pointer)
# Return values:
#   $v0 = the sum of the whole tree
# Registers modified: $t0
# Registers saved: $ra, $a0, $v0
sum_tree:
	bne $a0, 0, sum_branch	# do sum if not null
	li  $v0, 0		# return 0 if null
	jr  $ra			# return to caller
sum_branch:
	# get ready to do recursive call on left tree
	sw  $ra, ($sp)		# push return address
	sub $sp, $sp, 4		# on stack (-4 bytes)
	
	sw  $a0, ($sp)		# push root address
	sub $sp, $sp, 4		# on stack (-4 bytes)
	
	lw  $a0, 4($a0)		# load left sub-tree
	jal sum_tree 		# sum left sub-tree
	
	add $sp, $sp, 4		# pop (+ 4 bytes)
	lw  $a0, ($sp)		# root address

	lw  $t0, 0($a0)		# get label of root
	add $v0, $v0, $t0	# sum = left+root
	
	# get ready to do recursive call on right tree
	# remember: return address already on stack
	sw  $v0, ($sp)		# push sum (so far)
	sub $sp, $sp, 4		# on stack (-4 bytes)
	
	sw  $a0, ($sp)		# push root address
	sub $sp, $sp, 4		# on stack (-4 bytes)
	
	lw  $a0, 8($a0)		# load right sub-tree
	jal sum_tree 		# sum right sub-tree
	
	add $sp, $sp, 4		# pop (+4 bytes)
	lw  $a0, ($sp)		# root address
	
	add $sp, $sp, 4		# pop (+4 bytes)
	lw  $t0, ($sp)		# sum of left+root
	
	sub $v0, $t0, $v0	# sum=(left+root)-right
	
	# get ready to return
	add $sp, $sp, 4		# pop (+4 bytes)
	lw  $ra, ($sp)		# return address
	jr  $ra

# Print an integer to the user
# Arugments: $a0 (integer to print)
# Return values: NONE
# Registers modified: $v0
print_int:
	li	$v0, 1
	syscall
	jr	$ra

# Exit the program
# Arguments: NONE
# Return values: NONE (never returns)
# Registers modified: $v0 (doesn't matter)
exit:
	li	$v0, 10
	syscall
