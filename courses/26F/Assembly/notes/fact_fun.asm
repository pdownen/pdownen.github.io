	.text
main:
	jal	read_int	# v0 = read_int();
	move	$a0, $v0	# load argument
	jal	fact		# v0 = fact(a0);
	move	$a0, $v0	# load argument
	jal	print_int	# print(a0);
	jal	exit		# exit();



# Recursively calculate the factorial function
# Arguments: $a0
# Return values: $v0 (factorial of $a0)
# Registers modified: $v0
# Memory modified: $sp (stack pointer)
fact:	# int(v0) fact(int a0) {
	blez	$a0, fact0	#if (a0 > 0) {
	
	addi	$sp, $sp, -4	#  stack.grow(sizeof(int));
	sw	$s0, ($sp)	#  stack.push(s0);
	
	move	$s0, $a0	#  s0 = a0;
	addi	$a0, $a0, -1	#  a0--;
	
	addi	$sp, $sp, -4	#  stack.grow(sizeof(pointer));
	sw	$ra, ($sp)	#  stack.grow(&return);
	
	jal	fact		#  v0 = fact(a0);
	mul	$v0, $s0, $v0	#  v0 *= s0;
	
	lw	$ra, ($sp)	#  *return = stack[0];
	lw	$s0, 4($sp)	#  s0 = stack[1];
	addi	$sp, $sp, 8	#  stack.shrink(sizeof(struct {int; pointer}));
	jr	$ra		#  return v0;

fact0:				#} else {
	li	$v0, 1		#  v0 = 1;
	jr	$ra		#  return v0;
				#}
	# } end of fact



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
