.text
	beqz $a0, init_end
	lw $a0, 0($a1)
	jal atoi
	la $t0, arg
	sw $v0, 0($t0)
init_end:
	la $t0, arg
	lw $t0, 0($t0)
	sw $t0, 0($sp)
	subi $sp, $sp, 4
	jal main_int
	addi $sp, $sp, 4
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
print_int:
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
print_int_int:
	sw $fp, 0($sp)
	subi $sp, $sp, 4
	sw $ra, 0($sp)
	subi $sp, $sp, 4
	move $fp, $sp
	addi $fp, $fp, 8
	lw $a0, 4($fp)
	li $v0, 1
	syscall
	addi $sp, $sp, 4
	lw $ra, 0($sp)
	addi $sp, $sp, 4
	lw $fp, 0($sp)
	jr $ra
#power
power_int_int:
	sw $fp, 0($sp)
	subi $sp, $sp, 4
	sw $ra, 0($sp)
	subi $sp, $sp, 4
	move $fp, $sp
	addi $fp, $fp, 8
	lw $s0, 8($fp)
	lw $s1, 4($fp)
	li $t0, 1
	b power_loop_guard
power_loop_code:
	mul $t0, $t0, $s1
	subi $s0, $s0, 1
power_loop_guard:
	bgtz $s0, power_loop_code
	addi $sp, $sp, 4
	lw $ra, 0($sp)
	addi $sp, $sp, 4
	lw $fp, 0($sp)
	jr $ra
main_int:
	sw $fp, 0($sp)
	subi $sp, $sp, 4
	sw $ra, 0($sp)
	subi $sp, $sp, 4
	move $fp, $sp
	addi $fp, $fp, 8
	subi $sp, $sp, 0
	li $t0, -1
	sw $t0, 0($sp)
	subi $sp, $sp, 4
	la $t0, continuer
	addi $sp, $sp, 4
	lw $t1, 0($sp)
	sw $t1, 0($t0)
	li $t0, 0
	sw $t0, 0($sp)
	subi $sp, $sp, 4
	la $t0, i
	addi $sp, $sp, 4
	lw $t1, 0($sp)
	sw $t1, 0($t0)
_label_1:
	la $t0, continuer
	lw $t0, 0($t0)
	bne $zero, $t0, _label_2
	jal _label_3
_label_2:
	li $t0, 0
	sw $t0, 0($sp)
	subi $sp, $sp, 4
	la $t0, continuer
	addi $sp, $sp, 4
	lw $t1, 0($sp)
	sw $t1, 0($t0)
	li $t0, 0
	sw $t0, 0($sp)
	subi $sp, $sp, 4
	la $t0, j
	addi $sp, $sp, 4
	lw $t1, 0($sp)
	sw $t1, 0($t0)
_label_4:
	la $t0, j
	lw $t0, 0($t0)
	sw $t0, 0($sp)
	subi $sp, $sp, 4
	lw $t0, 4($fp)
	addi $t0, $t0, 1
	addi $sp, $sp, 4
	lw $t1, 0($sp)
	slt $t0, $t1, $t0
	bne $zero, $t0, _label_5
	jal _label_6
_label_5:
	la $t0, i
	lw $t0, 0($t0)
	sw $t0, 0($sp)
	subi $sp, $sp, 4
	la $t0, i
	lw $t0, 0($t0)
	addi $sp, $sp, 4
	lw $t1, 0($sp)
	mul $t0, $t0, $t1
	sw $t0, 0($sp)
	subi $sp, $sp, 4
	la $t0, j
	lw $t0, 0($t0)
	sw $t0, 0($sp)
	subi $sp, $sp, 4
	la $t0, j
	lw $t0, 0($t0)
	addi $sp, $sp, 4
	lw $t1, 0($sp)
	mul $t0, $t0, $t1
	addi $sp, $sp, 4
	lw $t1, 0($sp)
	add $t0, $t0, $t1
	sw $t0, 0($sp)
	subi $sp, $sp, 4
	lw $t0, 4($fp)
	sw $t0, 0($sp)
	subi $sp, $sp, 4
	lw $t0, 4($fp)
	addi $sp, $sp, 4
	lw $t1, 0($sp)
	mul $t0, $t0, $t1
	addi $sp, $sp, 4
	lw $t1, 0($sp)
	slt $t0, $t1, $t0
	bne $zero, $t0, _label_7
	li $t0, 35
	sw $t0, 0($sp)
	subi $sp, $sp, 4
	jal print_int
	addi $sp, $sp, 4
	jal _label_8
_label_7:
	li $t0, 46
	sw $t0, 0($sp)
	subi $sp, $sp, 4
	jal print_int
	addi $sp, $sp, 4
	li $t0, -1
	sw $t0, 0($sp)
	subi $sp, $sp, 4
	la $t0, continuer
	addi $sp, $sp, 4
	lw $t1, 0($sp)
	sw $t1, 0($t0)
_label_8:
	li $t0, 32
	sw $t0, 0($sp)
	subi $sp, $sp, 4
	jal print_int
	addi $sp, $sp, 4
	la $t0, j
	lw $t0, 0($t0)
	addi $t0, $t0, 1
	sw $t0, 0($sp)
	subi $sp, $sp, 4
	la $t0, j
	addi $sp, $sp, 4
	lw $t1, 0($sp)
	sw $t1, 0($t0)
	jal _label_4
_label_6:
	li $t0, 10
	sw $t0, 0($sp)
	subi $sp, $sp, 4
	jal print_int
	addi $sp, $sp, 4
	la $t0, i
	lw $t0, 0($t0)
	addi $t0, $t0, 1
	sw $t0, 0($sp)
	subi $sp, $sp, 4
	la $t0, i
	addi $sp, $sp, 4
	lw $t1, 0($sp)
	sw $t1, 0($t0)
	jal _label_1
_label_3:
	addi $sp, $sp, 0
	addi $sp, $sp, 4
	lw $ra, 0($sp)
	addi $sp, $sp, 4
	lw $fp, 0($sp)
	jr $ra
.data
j:
	.word 0
i:
	.word 0
continuer:
	.word 0
arg:
	.word 0
