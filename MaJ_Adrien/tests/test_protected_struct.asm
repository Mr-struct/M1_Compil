.text
	beqz $a0, init_end
	lw $a0, 0($a1)
	jal atoi
	la $t0, arg
	sw $v0, 0($t0)
init_end:
	li $t0, 2
	addi $a0, $a0, 1
	li $t1, 4
	mul $a0, $t0, $t1
	li $v0, 9
	syscall
	sw $t0, 0($v0)
	addi $t0, $v0, 4
	sw $t0, 0($sp)
	subi $sp, $sp, 4
	la $t0, t
	addi $sp, $sp, 4
	lw $t1, 0($sp)
	sw $t1, 0($t0)
	la $t0, t
	lw $t0, 0($t0)
	sw $t0, 0($sp)
	subi $sp, $sp, 4
	li $t0, 0
	addi $sp, $sp, 4
	lw $t1, 0($sp)
	lw $t2, -4($t1)
	sub $t2, $t0, $t2
	bgez $t2, atoi_error
	li $t2, 4
	mul $t0, $t0, $t2
	add $t0, $t1, $t0
	lw $t0, 0($t0)
	sw $t0, 0($sp)
	subi $sp, $sp, 4
	la $t0, x
	addi $sp, $sp, 4
	lw $t1, 0($sp)
	sw $t1, 0($t0)
	li $t0, 1
	sw $t0, 0($sp)
	subi $sp, $sp, 4
	la $t0, t
	lw $t0, 0($t0)
	sw $t0, 0($sp)
	subi $sp, $sp, 4
	li $t0, 1
	addi $sp, $sp, 4
	lw $t1, 0($sp)
	lw $t2, -4($t1)
	sub $t2, $t0, $t2
	bgez $t2, atoi_error
	li $t2, 4
	mul $t0, $t0, $t2
	add $t0, $t1, $t0
	addi $sp, $sp, 4
	lw $t1, 0($sp)
	sw $t1, 0($t0)
	la $t0, x
	lw $t0, 0($t0)
	move $a0, $t0
	li $v0, 1
	syscall
	la $t0, t
	lw $t0, 0($t0)
	sw $t0, 0($sp)
	subi $sp, $sp, 4
	li $t0, 0
	addi $sp, $sp, 4
	lw $t1, 0($sp)
	lw $t2, -4($t1)
	sub $t2, $t0, $t2
	bgez $t2, atoi_error
	li $t2, 4
	mul $t0, $t0, $t2
	add $t0, $t1, $t0
	lw $t0, 0($t0)
	move $a0, $t0
	li $v0, 1
	syscall
	la $t0, t
	lw $t0, 0($t0)
	sw $t0, 0($sp)
	subi $sp, $sp, 4
	li $t0, 1
	addi $sp, $sp, 4
	lw $t1, 0($sp)
	lw $t2, -4($t1)
	sub $t2, $t0, $t2
	bgez $t2, atoi_error
	li $t2, 4
	mul $t0, $t0, $t2
	add $t0, $t1, $t0
	lw $t0, 0($t0)
	move $a0, $t0
	li $v0, 1
	syscall
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
.data
x:
	.word 0
t:
	.word 0
arg:
	.word 0
