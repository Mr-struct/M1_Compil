.text
	beqz $a0, init_end
	lw $a0, 0($a1)
	jal atoi
	la $t0, arg
	sw $v0, 0($t0)
init_end:
	jal test_fun
	addi $sp, $sp, 0
	sw $t0, 0($sp)
	subi $sp, $sp, 4
	la $t0, x
	addi $sp, $sp, 4
	lw $t1, 0($sp)
	sw $t1, 0($t0)
	jal test_proc
	addi $sp, $sp, 0
	li $v0, 10
	syscall
atoi:
	move $t0, $a0
	li $t1, 0
	li $t3, 10
	li $t4, 48
	li $t5, 57
atoi_loop:
	lbu $t2, 0($t0)
	beq $t2, $zero, atoi_end
	blt $t2, $t4, atoi_error
	bgt $t2, $t5, atoi_error
	addi $t2, $t2, -48
	mul $t1, $t1, $t3
	add $t1, $t1, $t2
	addi $t0, $t0, 1
	b atoi_loop
atoi_error:
	li $v0, 10
	syscall
atoi_end:
	move $v0, $t1
	jr $ra
#print
print:
	sw $fp, 0($sp)
	subi $sp, $sp, 4
	sw $ra, 0($sp)
	subi $sp, $sp, 4
	move $fp, $sp
	addi $fp, $fp, 8
	lw $a0, 4($fp)
	li $v0, 11
	syscall
	addi $sp, $sp, 4
	lw $ra, 0($sp)
	addi $sp, $sp, 4
	lw $fp, 0($sp)
	jr $ra
#print_int
print_int:
	lw $a0, 4($fp)
	li $v0, 1
	syscall
	jr $ra
#power
power:
	lw $s0, 8($fp)
	lw $s1, 4($fp)
	li $t0, 1
	b power_loop_guard
power_loop_code:
	mul $t0, $t0, $s1
	subi $s0, $s0, 1
power_loop_guard:
	bgtz $s0, power_loop_code
	jr $ra
test_proc:
	sw $fp, 0($sp)
	subi $sp, $sp, 4
	sw $ra, 0($sp)
	subi $sp, $sp, 4
	move $fp, $sp
	addi $fp, $fp, 8
	subi $sp, $sp, 0
	li $t0, 92
	sw $t0, 0($sp)
	subi $sp, $sp, 4
	jal print
	addi $sp, $sp, 4
	addi $sp, $sp, 0
	addi $sp, $sp, 4
	lw $ra, 0($sp)
	addi $sp, $sp, 4
	lw $fp, 0($sp)
test_fun:
	sw $fp, 0($sp)
	subi $sp, $sp, 4
	sw $ra, 0($sp)
	subi $sp, $sp, 4
	move $fp, $sp
	addi $fp, $fp, 8
	subi $sp, $sp, 0
	li $t0, 92
	sw $t0, 0($sp)
	subi $sp, $sp, 4
	jal print
	addi $sp, $sp, 4
	li $t0, 92
	addi $sp, $sp, 0
	addi $sp, $sp, 4
	lw $ra, 0($sp)
	addi $sp, $sp, 4
	lw $fp, 0($sp)
	jr $ra
	addi $sp, $sp, 0
	addi $sp, $sp, 4
	lw $ra, 0($sp)
	addi $sp, $sp, 4
	lw $fp, 0($sp)
.data
x:
	.word 0
arg:
	.word 0
