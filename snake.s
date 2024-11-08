#
# CMPUT 229 Public Materials License
# Version 1.0
#
# Copyright 2020 University of Alberta
# Copyright 2022 Yufei Chen
# TODO: claim your copyright
# This software is distributed to students in the course
# CMPUT 229 - Computer Organization and Architecture I at the University of
# Alberta, Canada.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
# 1. Redistributions of source code must retain the above copyright notice,
#    this list of conditions and the disclaimer below in the documentation
#    and/or other materials provided with the distribution.
#
# 2. Neither the name of the copyright holder nor the names of its
#    contributors may be used to endorse or promote products derived from this
#    software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
# LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
# CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
# SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
# INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
# CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
# ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.
#
#-------------------------------
# Lab_Snake_Game Lab
#
# Author: Ian Morrison
# Date: November 2nd, 2022
# TA: James Thompson, Siva Chowdeswar Nandipati
# REFRENCES: displayDemo.s, Author: Zachary Selk
#-------------------------------

.include "common.s"

.data
.align 2

TIME:			.word 0xFFFF0018
TIMECMP:		.word 0xFFFF0020
KEYBOARD_CONTROL: 	.word 0xFFFF0000
KEYBOARD_DATA:	 	.word 0xFFFF0004
DISPLAY_CONTROL:    	.word 0xFFFF0008
DISPLAY_DATA:       	.word 0xFFFF000C

INTERRUPT_ERROR:		.asciz "Error: Unhandled interrupt with exception code: "
INSTRUCTION_ERROR:		.asciz "\n   Originating from the instruction at address: "
gameInstructions:		.asciz "Please enter a 1, 2, or 3 to choose the level and start the game"
snakeString:			.asciz "@***"
pointsString:			.asciz "points"
secondsString:			.asciz "seconds"
stringClear:			.asciz "                                                        "
Brick:      			.asciz "#"

gameTime:		.word 0
points:			.word 0

validDirection:		.word 0
gameStart:		.word 0
gameNumber: 		.word 0
bonusTime:		.word 0
rowDirection: 		.word 0
colDirection:		.word -1
screenDraw:		.word 0
wallHit:		.word 1
timesUp:		.word 0
appleLocation:		.word 0

rowMax:			.word 10
colMax:			.word 20


.align 2
	snake:			
			.word 0x00080005
			.word 0x00090005
			.word 0x000A0005
			.word 0x000B0005

.text


#------------------------------------------------------------------------------
# snakeGame
#
#  This function operates the game logic for our snake game. 
# The game has two phases: 	gameLevel, where we prompt user for an input of 1, 2, or 3. 
#					     	gamePlay, where then main game loop operates by waiting for user to change snake direction
#						with w, a, s, or d. Game waits for timer interupt every second to update screen and check for wall hits,
#						apple eating, and game time remaining.
#
#
# Register Usage:
#	t0 - Used to setup control and status registers
#	t1, t2, t3 - Hold addresses and/or values for keyboard/display interupt manipulation 
#------------------------------------------------------------------------------
snakeGame:

	addi t0, zero, 1
	csrrw t0, 0x00, t0	# Sets Ustatus to 1
	addi t0, zero, 0x0100
	csrrw t0, 0x04, t0	# Sets UIE to enable  keyboard interrupts
	la t0, handler
	csrrw t0, 0x05, t0	# Sets Utvec to address of handler

	addi t0, zero, 2
	lw t1, KEYBOARD_CONTROL	
	sw t0, 0(t1)				# Sets keyboard control to 1, allowing for keyboard interrupts
	la a0, gameInstructions			# a0 <- Address on instruction string
	addi a1, zero, 0
	addi a2, zero, 0
	jal printStr
	
	gameLevel:
		lw t0, gameStart		# t0 <- gameStart
		beqz t0, gameLevel

		la a0, stringClear
		addi a2, zero, 21
		jal printStr			# Clears prompt text

		lw t1, TIMECMP
		lw t2, TIME
		lw t2, 0(t2)			# t2 <- current program time
		addi t2, t2, 1000		# t2 <- program time + 1000ms
		sw t2, 0(t1)			# timecmp <- program time + 1000ms
		
		addi t3, zero, 1		# t3 <- 1
		lw t4, DISPLAY_CONTROL
		sw t3, 0(t4)			# Sets 0th bit in display control to 1 so we can write to screen
		jal printAllWalls
		la a0, snakeString		# a0 <- Address of snake string
		addi a1, zero, 5		# a1 <- Row 5
		addi a2, zero, 8		# a2 <- Column 8
		jal printStr			# Prints starting snake

		jal newApple			# Generates random apple location
		jal drawApple			# Draws apple
		la a0, gameTime
		mv a1, zero
		jal drawPointsandTime		# Draws starting game time
		la a0, secondsString		# Draws "seconds"
		mv a1, zero
		addi a2, zero, 27
		jal printStr			# Draws "seconds"
		
		la a0, points
		addi a1, zero, 1
		jal drawPointsandTime		# Draws starting points total
		la a0, pointsString		
		addi a1, zero, 1
		addi a2, zero, 27
		jal printStr			# Draws "points"
		
		lw t0, DISPLAY_CONTROL
		sw zero, 0(t0)			# Resets Display Control to 0

		addi t0, zero, 0x0110			
		csrrw t0, 0x04, t0		# Sets UIE to enable timer and keyboard interrupts
		
	gamePlay:
		jal drawSnake			# Draws snake
		jal drawApple			# Draws apple		
		la a0, gameTime
		mv a1, zero
		jal drawPointsandTime		# Draws current game time	
		la a0, points
		addi a1, zero, 1
		jal drawPointsandTime		# Draws current points total
		lw t0, DISPLAY_CONTROL
		sw zero, 0(t0)			# Resets Display Control to 0
		jal checkHit			# Check to see if snake has hit a wall
		
		timeCheck:
			lw t1, gameTime
			slti t2, t1, 1
			bnez t2, gameEnd 	# If game time is less than 1, game will end
			la t3, validDirection
			sw zero, 0(t3)		# Resets validDirection
			j gamePlay		# Game keeps playing if no end conditions are met

gameEnd:

	# Turns off all user exceptions/interupts
	csrrw zero, 0x00, zero	
	csrrw zero, 0x04, zero
	j gameEnd


#------------------------------------------------------------------------------
# drawSnake
#
#  This function draws our snake at an updated location, based on last user input
#
# Register Usage:
#	s2 - Address of snake array
#	s3 - Counter for snake loop
#	s5 - Holds new location snake segment location
#	s6 - Holds current or previous snake segment location
#	s7 - Holds snake tail location
#	t1 - Holds row location
#	t2 - Holds column location
#	t3 - Holds row direction
#	t4 - Holds column direction
#------------------------------------------------------------------------------
drawSnake:

	la s2, snake						# Loads address of snake array into s2
	lw t3, rowDirection					# Loads row direction modifier into  t3
	lw t4, colDirection					#  Loads column direciton modifier into t4
	
	lw s6, 0(s2)						# s6 <- Location of snake head
	lw s7, 12(s2)						# s7 <- Location of snake tail
	
	lb t1, 0(s2)						# t1 <- Current row location
	add t1, t1, t3						# Adds row modifier to current row location
	lb t2, 2(s2)						# t2 <- Current column location
	add t2, t2, t4						# Adds column modifier to current column location
	slli t2, t2, 16
	
	or s5, t2, t1						# s5 <- New snake head location
	sw s5, 0(s2)						# s2 <- New location of snake head
	mv a0, s5
	li a0, 64							# Loads '@' char into a1
	mv a1, t1
	srli a2, t2, 16

	# save ra
	addi sp, sp -4
	sw ra, 0(sp)
	jal printChar
	# restore ra
	lw ra, 0(sp)
	addi sp, sp 4

	li a0, 42							# Loads '*' char into a0
	li s3, 3							# s3 <- Counter set to 3
		
		snakeLoop:
			andi a1, s6, 0x0000000F
			srli a2, s6, 16
			
			# save ra
			addi sp, sp -4
			sw ra, 0(sp)
			jal printChar
			# restore ra
			lw ra, 0(sp)
			addi sp, sp 4
			
			mv s5, s6
			addi s2, s2, 4

			lw s6, 0(s2)		# Loads next snake segment into s6
			sw s5, 0(s2)		# 0(s2) <- Location of current snake body segment
			addi s3, s3, -1		# Subtracts 1 from counter
			bnez s3, snakeLoop	# If s3 != zero, branch to snakeLoop as there are body segments remaining		
			
			addi a0, zero, 32
			andi a1, s7, 0x0000000F
			srli a2, s7, 16
			# save ra
			addi sp, sp -4
			sw ra, 0(sp)
			jal printChar
			# restore ra
			lw ra, 0(sp)
			addi sp, sp 4
			ret


#------------------------------------------------------------------------------
# drawPointsandTime
#
#  This function draws our current game time and points total
#
# Register Usage:
#	s2 - Value to print
#	s3 - "ones" value
#	s4 - "tens" value
#	s5 - "hundreds" value
#	s6 - Row offset
#	t1 - Holds 10 for modulo operation
#	t2 - Holds 100 for modulo operation
#------------------------------------------------------------------------------
drawPointsandTime:

# Save return address
	addi sp, sp, -4
	sw ra, 0(sp)
	
	lw s2, 0(a0)		# s2 <- value to print
	mv s6, a1		# s6 <- Row offset
	li t1, 10
	rem s3, s2, t1		# s3 <- "ones" value
	sub s2, s2, s3		# s2 <- "hundreds" and "tens" values 
	li t2, 100
	rem s4, s2, t2		# s4 <- xx0 Modulo 100	
	sub s2, s2, s4		# s2 <- x00
	div s4, s4, t1		# s4 <- "tens" value
		
	div s5, s2, t2		#  s5 <- "hundreds" value
	
	mv a0, s5
	addi a0, a0, 48
	add a1, zero, s6	# Row offset
	addi a2, zero, 23
	jal printChar		# Prints "hundreds" value
	
	mv a0, s4
	addi a0, a0, 48
	add a1, zero, s6	# Row offset
	addi a2, zero, 24
	jal printChar		# Prints "tens" value

	mv a0, s3
	addi a0, a0, 48
	add a1, zero, s6	# Row offset
	addi a2, zero, 25	# Prints "ones" value
	jal printChar
	
	# Restore return address
	lw ra, 0(sp)
	addi sp, sp, 4
	ret


#------------------------------------------------------------------------------
# checkHit
#
#  This function checks if our snake head is in or outside our walls
#
# Register Usage:
#	t0 - Address of snake array
#	t1 - Snake head row location
#	t2 - Mask
#	t3 - Row maximum
#	t4 - Snake head location
#	t5 - Snake head column location 
#------------------------------------------------------------------------------
checkHit:

	la t0, snake
	lh t1, 0(t0)			# Loads row location of snake head into t1
	lw t3, rowMax			# t3 <- rowMax
	bge t1, t3, hit			# If snake head row loaction is in, or exceeds upper wall boundary, a hit has occured
	ble t1, zero, hit		# If snake haed row location is in or exceeds lower wall boundary, a hit has occured
		
	lw t3, colMax			# t3 <- colMax
	lw t4, 0(t0)			# t4 <- Snake Head Location
	li t2, 0xFFFF0000		# Mask to get Col Location
	and t5, t4, t2			# Column location of snake head
	srli t5, t5, 16			# Puts column location into start of word
	bge t5, t3, hit			# If snake head col location is in or exceeds upper wall boundary, a hit has occured
	ble t5, zero, hit		# If snake head col loaction is in or exceeds lower wall boundary, a hit has occured
		
		checkEat:

			lw t6, appleLocation		# t6 <- appleLocation
			bne t4, t6, timeCheck		# If apple and snake head coordinates do not match, branch to timeCheck
			
			lw t0, points			# t0 <- points
			addi t0, t0, 1			# Adds 1 to our point total
			sw t0, points, t1		# Stores updated point total into points
			
			lw t0, bonusTime		# t0 <- bonusTime
			lw t1, gameTime			# t1 <- gameTime
			add t1, t1, t0			# Adds bonus time to game time
			sw t1, gameTime, t2		# Stores updated game time into gameTime
			
			# Save return address
			addi sp, sp, -4
			sw ra, 0(sp)
			jal newApple			# Generates new apple location data			
			# Restore return address
			lw ra, 0(sp)
			addi sp, sp, 4		
			ret
		
hit:
	la t0, wallHit
	addi t1, zero, 1
	sw t1,  0(t0)				# Sets wallHit to 1 so game will end
	j gameEnd				# Ends game


#------------------------------------------------------------------------------
# newApple
#
#  This function generates a new apple location and stores it in appleLocation
#
# Register Usage:
#	s2 - New apple location
#	t0 - Column location
#	t1 - appleLocation address
#------------------------------------------------------------------------------
newApple:
	# Save registers
	addi sp, sp, -4
	sw ra, 0(sp)
	
	jal random
	addi s2, a0, 1			# Adds 1 to our random number 
	jal random

	addi a0, a0, 1			# Adds 1 to our random number
	slli t0, a0, 16			# Shifts Col data into upper word
	mv a1, s2			# a1 <- Row
	or s2, s2, t0			# Col location is in bits 16-31, row locatoin is in bits 0 - 15
	la t1, appleLocation
	sw s2, 0(t1)			# Updates our new apple location
	
	# Restore registers
	lw ra, 0(sp)
	addi sp, sp, 4
	ret


#------------------------------------------------------------------------------
# drawApple
#
#  This function draws our apple to screen
#
# Register Usage:
#	a0 - 'a' character ASCII value
#	a1 - Apple row
#	a2 - Apple column
#	s1 - Apple location
#	t0 - Row and Column masks
#------------------------------------------------------------------------------
drawApple:
	# Save registers
	addi sp, sp, -4
	sw ra, 0(sp)
	
	addi a0, zero, 0x061		# a0 <- 'a' ASCII value	
	lw s1, appleLocation		# s1 <- apple location
	li t0, 0x0F			# t0 <- Row mask
	and a1, s1, t0			# a1 <- Row
	li t0, 0x000F0000		# t0 <- Column mask
	and a2, s1, t0			# a2 <- Column
	srli a2, a2, 16			# a2 <- Column location shited to bits 0-7
	
	# Save registers
	addi sp, sp, -4
	sw ra, 0(sp)
	jal printChar			# Draws apple
	# Restore registers
	lw ra, 0(sp)
	addi sp, sp, 4
	ret
	

#------------------------------------------------------------------------------
# random
#
#  This function generates a random number given  three constant variables, and a seed, then updates seed with generated number
#
# Args:
#	a0 - Returns random number
#	
# Register Usage:
#	t0 - XiVar (seed)
#	t1 - Constant a
#	t2 - Constant c
#	t3 - Constant m
#------------------------------------------------------------------------------
random:
	lw t0, XiVar	# Load value of XiVar into t0
	lw t1, aVar	# Load value of aVar into t1 
	lw t2, cVar	# Load value of cVar into t2
	lw t3, mVar	# Load value of mVar into t3
	
	mul a0, t0, t1	# a0 <- Product of Xi and a
	add a0, a0, t2	# a0 <- Sum of a0 and c
	rem a0, a0, t3 	# a0 <- Remainder of a0 / m
	
	la t0, XiVar
	sw a0, 0(t0)	# Updates seed
ret


#------------------------------------------------------------------------------
# handler
#
#  This is our exception handler. It checks for user timer interupts, and external user interupts (keyboard input), and updates global variables accordingly.
#	'1', '2', and '3' are all valid inputs for the gameLevel phase of the game
#	'w', 'a', 's', 'd' are all valid inputs for the gamePlay phase of the game
#	If any other input is found, handler will return. Else, it loads corresponding level, or updates snake direction.
#	All other exceptions/interupts result in termination
#	
# Register Usage:
#	t0 - Used to update/clear control and status registers
#	t1 - Mask to get bit 31
#	t2 - Holds immedates to check for user interupts
#------------------------------------------------------------------------------
handler:
  	csrrw a0, 0x040, a0		# Swaps Usera0 and uscratch
  	# Save registers
  	sw t0, 0(a0)
   	sw t1, 4(a0)
   	sw t2, 8(a0)
   	sw t3, 12(a0)
   	sw t4, 16(a0)
   	sw t5, 20(a0)
   	sw t6, 24(a0)
   	sw s2, 28(a0)
   	sw s3, 32(a0)
   	
   	csrr t0, 0x040		# Stores Usera0 in t0
   	sw t0, 36(a0)		# Saves Usera0
   
   	csrrw t0, 0x42, zero		# Stores cause in t0 and clears cause
   	li t1, 0x7FFFFFFF
   	and t1, t0, t1			# Gets bits 30-0 from cause into t1
   	srli t0, t0, 31			# Puts bit 31 into bit 0 to check for Exceptions or Interrupts
   	addi t0, t0, -1			# Subtracts 1 from  t0
   	bnez	t0, handlerTerminate # If t0 was 1, keep going. Else, terminate handler
   	
   	li t2, 4			# Loads 4 to check for User Time Interupts
   	beq t2, t1, timer
   	li t2, 8			# Loads 6 to check for Usert External Interrupt
   	beq t2, t1, keypress
   	j handlerTerminate


#------------------------------------------------------------------------------
# keypress
#
#  This function determines where the user key-press is a valid level or direction choice
#
# Register Usage:
#	t0 - Holds '2' to reset keyboard control, and ASCII values for valid input
#	t1 - Holds address of keyboard data as well as ASCII value of input key
#	t2 - Holds row modifier or game time value
#	t3 - Holds column modifier
#	t4 - Holds bonus time value
#	t5 - Holds global variable addresses
#------------------------------------------------------------------------------
keypress: 
	addi t0, zero, 2
	lw t1, KEYBOARD_CONTROL
	sw t0, 0(t1)			# Resets keyboard control to accept new interrupts
 	lw t0, gameStart
 	lw t1, KEYBOARD_DATA		
   	lw t1, 0(t1)			# t1 <- Value (ASCII CHAR)
   	beq zero, t0, levelChoice
   	lw t6, validDirection
   	bnez t6, complete

   	li t0, 0x077			# t0 <- ASCII 'w'
   	li t2, -1			# Row - 1
   	li t3, 0			# Col + 0
   	beq t0, t1, directionChosen
   	
   	li t0, 0x061			# t0 <- ASCII 'a'
   	li t2, 0			# Row + 0
  	li t3, -1			# Col - 1
  	beq t0, t1, directionChosen
  	
   	li t0, 0x073			# t0 <- ASCII 's'
   	li t2, 1			# Row + 1
   	li t3, 0			# Col + 0
   	beq t0, t1, directionChosen
   			
   	li t0, 0x064			# t0 <- ASCII 'd'
   	li t2, 0			# Row + 0
   	li t3, 1			# Col + 1
   	beq t0, t1, directionChosen
   	j complete
   		
   	levelChoice:
   		li t3, 120			# t3 <- 120
   		li t4, 8			# t4 <- 8
   	 	li t0, 0x031			# t0 <- ASCII '1'
   	 	beq t0, t1, levelChosen		# If User input == 1, branched to level picked			

   		li t3, 030			# t3 <- 30
   		li t4, 5			# t4 <- 5
   		li t0, 0x032			# t0 <- ASCII '2'
		beq t0, t1, levelChosen		# If User input == 2, branched to level picked

   		li t3, 015			# t3 <- 15
   		li t4, 3			# t4 <- 3		
   		li t0, 0x033			# t0 <- ASCII '3'
		beq t0, t1, levelChosen		# If User input == 3, branched to level picked
		j complete			# If user input is not '1'. '2', or '3', jump to complete
		
	levelChosen:
		la t5, gameTime
		sw t3, 0(t5)			# Stores selected game time in gameTime
		la t5, bonusTime
		sw t4, 0(t5)			# Stores selected bonus time value into bonusTime
		la t5, gameStart
		sw t4, 0(t5)			# Stores t4 into gameStart to break loop
		j complete
	
	directionChosen:
		la t5, rowDirection
		sw t2, 0(t5)		# Stores direction in rowDirection
		la t5, colDirection
		sw t3, 0(t5)		# Stores direction in colDirection
		addi t0, zero, 1	
		la t1, validDirection
		sw t0, 0(t1)		# Sets validDirection to 1
		j complete


#------------------------------------------------------------------------------
# timer
#
#  This function update game time and resets display control every second
#
# Register Usage:
#	t0 - Addresses of globalVariables or time/timecmp
#	t1 - time values
#	t2 - Address of timecmp
#	t3 - Address of display control
#	t4 - 1 to reset display control
#------------------------------------------------------------------------------	
timer:
	la t0, gameTime
	lw t1, 0(t0)
	addi t1, t1, -1
	sw t1, 0(t0)		# gameTime -= 1
	
	lw t0, TIME		# t0 <- Address of time
	lw t1, 0(t0)		# t0 <- Value in time
	lw t2, TIMECMP	# t2 <- Address of timecmp
	addi t1, t1, 1000	# t1 <- Program time + 1 second
	sw t1, 0(t2)		# timecmp <- t1
	
	addi t4, zero, 1
	lw t3, DISPLAY_CONTROL
	sw t4, 0(t3)		# Sets display control bit 0 to 1 so we can draw to screen
	j complete


complete:
   	csrrsi t0, 0x41, 0		# Copies our User Exceptions Program Counter into t0
  	#addi t0, t0, 4			# Adds 4 to our counter so we don't start an infinite loop
   	csrrw t1, 0x41, t0		# Swaps new counter into User Exceptions Program Counter register
   
  	 lw t0, 36(a0)		# Loads usera0 into t0
  	csrw t0, 0x040		# Puts usera0 into uscratch
   
  	 # Restore user registers
  	lw t0, 0(a0)
   	lw t1, 4(a0)
   	lw t2, 8(a0)
   	lw t3, 12(a0)
   	lw t4, 16(a0)
   	lw t5, 20(a0)
   	lw t6, 24(a0)
   	lw s2, 28(a0)
   	lw s3, 32(a0)
   	csrrw a0, 0x040, a0		# Swaps usera0 and uscratch, restoring a0
	uret


handlerTerminate:
	# Print error msg before terminating
	li     a7, 4
	la     a0, INTERRUPT_ERROR
	ecall
	li     a7, 34
	csrrci a0, 66, 0
	ecall
	li     a7, 4
	la     a0, INSTRUCTION_ERROR
	ecall
	li     a7, 34
	csrrci a0, 65, 0
	ecall

handlerQuit:
	li     a7, 10
	ecall	# End of program


#---------------------------------------------------------------------------------------------
# printAllWalls
#
# Subroutine description: This subroutine prints all the walls within which the snake moves
# 
#   Args:
#  		None
#
# Register Usage
#      s0: the current row
#      s1: the end row
#
# Return Values:
#	None
#---------------------------------------------------------------------------------------------
printAllWalls:
	# Stack
	addi   sp, sp, -12
	sw     ra, 0(sp)
	sw     s0, 4(sp)
	sw     s1, 8(sp)
	# print the top wall
	li     a0, 21
	li     a1, 0
	li     a2, 0
	la     a3, Brick
	lbu    a3, 0(a3)
	jal    ra, printMultipleSameChars

	li     s0, 1	# s0 <- startRow
	li     s1, 10	# s1 <- endRow
printAllWallsLoop:
	bge    s0, s1, printAllWallsLoopEnd
	# print the first brick
	la     a0, Brick	# a0 <- address(Brick)
	lbu    a0, 0(a0)	# a0 <- '#'
	mv     a1, s0		# a1 <- row
	li     a2, 0		# a2 <- col
	jal    ra, printChar
	# print the second brick
	la     a0, Brick
	lbu    a0, 0(a0)
	mv     a1, s0
	li     a2, 20
	jal    ra, printChar
	
	addi   s0, s0, 1
	jal    zero, printAllWallsLoop

printAllWallsLoopEnd:
	# print the bottom wall
	li     a0, 21
	li     a1, 10
	li     a2, 0
	la     a3, Brick
	lbu    a3, 0(a3)
	jal    ra, printMultipleSameChars

	# Unstack
	lw     ra, 0(sp)
	lw     s0, 4(sp)
	lw     s1, 8(sp)
	addi   sp, sp, 12
	jalr   zero, ra, 0


#---------------------------------------------------------------------------------------------
# printMultipleSameChars
# 
# Subroutine description: This subroutine prints white spaces in the Keyboard and Display MMIO Simulator terminal at the
# given row and column.
# 
#   Args:
#   a0: length of the chars
# 	a1: row - The row to print on.
# 	a2: col - The column to start printing on.
#   a3: char to print
#
# Register Usage
#      s0: the remaining number of cahrs
#      s1: the current row
#      s2: the current column
#      s3: the char to be printed
#
# Return Values:
#	None
#---------------------------------------------------------------------------------------------
printMultipleSameChars:
	# Stack
	addi   sp, sp, -20
	sw     ra, 0(sp)
	sw     s0, 4(sp)
	sw     s1, 8(sp)
	sw     s2, 12(sp)
	sw     s3, 16(sp)

	mv     s0, a0
	mv     s1, a1
	mv     s2, a2
	mv     s3, a3

# the loop for printing the chars
printMultipleSameCharsLoop:
	beq    s0, zero, printMultipleSameCharsLoopEnd   # branch if there's no remaining white space to print
	# Print character
	mv     a0, s3	# a0 <- char
	mv     a1, s1	# a1 <- row
	mv     a2, s2	# a2 <- col
	jal    ra, printChar
		
	addi   s0, s0, -1	# s0--
	addi   s2, s2, 1	# col++
	jal    zero, printMultipleSameCharsLoop

# All the printing chars work is done
printMultipleSameCharsLoopEnd:	
	# Unstack
	lw     ra, 0(sp)
	lw     s0, 4(sp)
	lw     s1, 8(sp)
	lw     s2, 12(sp)
	lw     s3, 16(sp)
	addi   sp, sp, 20
	jalr   zero, ra, 0


#------------------------------------------------------------------------------
# printStr
#
# Subroutine description: Prints a string in the Keyboard and Display MMIO Simulator terminal at the
# given row and column.
#
# Args:
# 	a0: strAddr - The address of the null-terminated string to be printed.
# 	a1: row - The row to print on.
# 	a2: col - The column to start printing on.
#
# Register Usage
#      s0: The address of the string to be printed.
#      s1: The current row
#      s2: The current column
#      t0: The current character
#      t1: '\n'
#
# Return Values:
#	None
#
# References: This peice of code is adjusted from displayDemo.s(Zachary Selk, Jul 18, 2019)
#------------------------------------------------------------------------------
printStr:
	# Stack
	addi   sp, sp, -16
	sw     ra, 0(sp)
	sw     s0, 4(sp)
	sw     s1, 8(sp)
	sw     s2, 12(sp)

	mv     s0, a0
	mv     s1, a1
	mv     s2, a2

# the loop for printing string
printStrLoop:
	# Check for null-character
	lb     t0, 0(s0)
	# Loop while(str[i] != '\0')
	beq    t0, zero, printStrLoopEnd

	# Print Char
	mv     a0, t0
	mv     a1, s1
	mv     a2, s2
	jal    ra, printChar

	addi   s0, s0, 1	# i++
	addi   s2, s2, 1	# col++
	jal    zero, printStrLoop

printStrLoopEnd:
	# Unstack
	lw     ra, 0(sp)
	lw     s0, 4(sp)
	lw     s1, 8(sp)
	lw     s2, 12(sp)
	addi   sp, sp, 16
	jalr   zero, ra, 0



#------------------------------------------------------------------------------
# printChar
#
# Subroutine description: Prints a single character to the Keyboard and Display MMIO Simulator terminal
# at the given row and column.
#
# Args:
# 	a0: char - The character to print
#	a1: row - The row to print the given character
#	a2: col - The column to print the given character
#
# Register Usage
#      s0: The character to be printed.
#      s1: the current row
#      s2: the current column
#      t0: Bell ascii 7
#      t1: DISPLAY_DATA
#
# Return Values:
#	None
#
# References: This peice of code is adjusted from displayDemo.s(Zachary Selk, Jul 18, 2019)
#------------------------------------------------------------------------------
printChar:
#ebreak
	# Stack
	addi   sp, sp, -16
	sw     ra, 0(sp)
	sw     s0, 4(sp)
	sw     s1, 8(sp)
	sw     s2, 12(sp)
	# save parameters
	mv     s0, a0
	mv     s1, a1
	mv     s2, a2

	jal    ra, waitForDisplayReady

	# Load bell and position into a register
	addi   t0, zero, 7		# Bell ascii
	slli   s1, s1, 8			# Shift row into position
	slli   s2, s2, 20			# Shift col into position
	or     t0, t0, s1
	or     t0, t0, s2			# Combine ascii, row, & col
	
	# Move cursor
	lw     t1, DISPLAY_DATA
	sw     t0, 0(t1)
	jal    waitForDisplayReady	# Wait for display before printing
	
	# Print char
	lw     t0, DISPLAY_DATA
	sw     s0, 0(t0)
	
	# Unstack
	lw     ra, 0(sp)
	lw     s0, 4(sp)
	lw     s1, 8(sp)
	lw     s2, 12(sp)
	addi   sp, sp, 16
	jalr   zero, ra, 0



#------------------------------------------------------------------------------
# waitForDisplayReady
#
# Subroutine description: A method that will check if the Keyboard and Display MMIO Simulator terminal
# can be writen to, busy-waiting until it can.
#
# Args:
# 	None
#
# Register Usage
#      t0: used for DISPLAY_CONTROL
#
# Return Values:
#	None
#
# References: This peice of code is adjusted from displayDemo.s(Zachary Selk, Jul 18, 2019)
#------------------------------------------------------------------------------
waitForDisplayReady:
	# Loop while display ready bit is zero
	lw     t0, DISPLAY_CONTROL
	lw     t0, 0(t0)
	andi   t0, t0, 1
	beq    t0, zero, waitForDisplayReady
	jalr   zero, ra, 0
