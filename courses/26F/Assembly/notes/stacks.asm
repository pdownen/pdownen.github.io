	.data
stack:	.word	0:80

	.text
main:
	la  $a0, stack
	li  $a1, 1
	jal push_byte
	li  $a1, 2
	jal push_byte
	jal align_stack
	li  $a1, 3
	jal push_word
	jal pop_word
	move $s1, $v0
	sub $a0, $a0, 2
	jal pop_byte
	add $s1, $s1, $v0
	jal pop_byte
	add $s1, $s1, $v0
	move $s0, $a0
	move $a0, $s1
	jal print_int
	jal exit

# Push a whole word onto a stack
# Arguments:
#   $a0 (current stack pointer)
#   $a1 (word value to push)
# Return values:
#   $a0 (updated stack pointer)
# Registers modified: NONE
push_word:
	sw  $a1, ($a0)
	add $a0, $a0, 4
	jr  $ra

# Push a single byte onto the stack
# Arguments:
#   $a0 (current stack pointer)
#   $a1 (byte value to push)
# Return values:
#   $a0 (updated stack pointer)
# Registers modified: NONE
push_byte:
	sb  $a1, ($a0)
	add $a0, $a0, 1
	jr  $ra

# Pop a whole word from a stack
# Arguments:
#   $a0 (current stack pointer)
# Return values:
#   $a0 (updated stack pointer)
#   $v0 (popped word value)
# Registers modified:
pop_word:
	sub $a0, $a0, 4
	lw  $v0, ($a0)
	jr  $ra

# Pop a single byte from a stack
# Arguments:
#   $a0 (current stack pointer)
# Return values:
#   $a0 (updated stack pointer)
#   $v0 (popped byte value)
# Registers modified: NONE
pop_byte:
	sub $a0, $a0, 1
	lb  $v0, ($a0)
	jr  $ra

# Pad a stack to the next even word alignment
# Arguments:
#   $a0 (current stack pointer)
# Return values:
#   $a0 (aligned stack pointer)
# Registers modified: $t0
align_stack:
	and $t0, $a0, 0x00000003
	beq $t0, 0, now_aligned
	sra $a0, $a0, 2
	add $a0, $a0, 1
	sll $a0, $a0, 2
now_aligned:
	jr  $ra


# Read an integer from the user
# Arguments: NONE
# Return values: $v0 (integer read)
# Registers modified: $v0
read_int:
	# int(v0) read_int() {
	li	$v0, 5		# v0 = 5; // read integer code
	syscall			# v0 = system_read_int();
	jr	$ra		# return v0;
	# } end of read_int



# Print an integer to the user
# Arugments: $a0 (integer to print)
# Return values: NONE
# Registers modified: $v0
print_int:
	# void print_int(int a0) {
	li	$v0, 1		# v0 = 1; // print integer code
	syscall			# system_print_int(a0);
	jr	$ra		# return;
	# } end print_int
	# note, v0 is still set to 1



# Exit the program
# Arguments: NONE
# Return values: NONE (never returns)
# Registers modified: $v0 (doesn't matter)
exit:	# void exit() {
	li	$v0, 10		# v0 = 10; // exit code
	syscall			# system_exit();
	# } end of exit
