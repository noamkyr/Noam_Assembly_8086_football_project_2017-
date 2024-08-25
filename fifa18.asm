;Noam Kyram Y2 Final project.
;June 2017


IDEAL
MODEL small
STACK 100h
DATASEG
; --------------------------
; Your variables here
; --------------------------

rowGk dw ?
colGk dw ?
posGk dw ?
 

pos dw ? ;holds the current position of the ball
col dw ? ; holds the current column of the ball
row dw ? ; holds the current row of the ball in the screen 
count db 0 ;holds how much times the goal keeper saved the ball
msgGoal db 13,10,'goal! Your time is: ','$' ;the message which appears when the ball is in the goal.
saveOK db 0d
filename db 'open.bmp',0 ; the file name of the opening bmp
filehandle dw ?
Header db 54 dup(0)
Palette db 256*4 dup (0) 
ScrLine db 320 dup (0)
ErrorMsg1 db 'Error1', 13, 10,'$' 
ErrorMsg2 db 'Error2', 13, 10,'$' ;check for eror in each bmp 
ErrorMsg3 db 'Error3', 13, 10,'$' 
filename1 db 'end.bmp',0 ; the file name of the ending bmp when there is a goal
filename2 db 'done.bmp',0 ; the file name of the ending bmp when the player wins

divisorTable   db   10,  1,  0

startSec db ?  ;the current second when the game start
startMin db ? ;the current minute  when the game start 
totalSec dw ? ;the sum of the total sum of seconds when the game starts = startMin*60 +startSec

finSec db ?  ;the current second when the game ends
finMin db ?  ;the current minute  when the game ends
totalFin dw ? ;the sum of the total sum of seconds when the game starts = finMin*60 +finSec

CODESEG
; --------------------------
; Your code here - Procedures
; --------------------------

proc printBMP
	push ax
	push bx
	push cx
	push dx
	push es
	push si

	call OpenBMP 
	call ReadHeader
	call ReadPalette
	call CopyPal
	call CopyBitmap
	
	pop si
	pop es
	pop dx 
	pop cx 
	pop bx
	pop ax 
	ret
endp printBMP


proc OpenBMP 
	;push ax
	; Open file
	mov ah, 3Dh
	xor al, al 
	int 21h
	jc openerrorBMP
	mov [filehandle], ax

	;pop ax 
	ret 
	
openerrorBMP :
	mov dx, offset ErrorMsg1 
	mov ah, 9h
	int 21h

	;pop ax 
	ret
	
endp OpenBMP



proc OpenFile 
	push ax
	push dx
	; Open file
	mov ah, 3Dh
	xor al, al 
	mov dx, offset filename 
	int 21h
	jc openerror
	mov [filehandle], ax

	pop dx
	pop ax 
	ret 
	
openerror :
	mov dx, offset ErrorMsg1 
	mov ah, 9h
	int 21h

	pop dx
	pop ax 
	ret
	
endp OpenFile

proc ReadHeader 
	; Read BMP file header, 54 bytes
	
	mov ah,3fh 
	mov bx, [filehandle]
	mov cx,54 
	mov dx,offset Header
	int 21h
	
	ret
endp ReadHeader 

proc ReadPalette 
	
	
	; Read BMP file color palette, 256 colors * 4 bytes (400h)
	mov ah ,3fh
	mov cx,400h
	mov dx,offset Palette 
	int 21h
	

	ret
endp ReadPalette 

proc CopyPal
	; Copy the colors palette to the video memory
	; The number of the first color should be sent to port 3C8h
	; The palette is sent to port 3C9h

	mov si,offset Palette	
	mov cx,256 							
	mov dx,3C8h
	mov al,0
	; Copy starting color to port 3C8h
	out dx,al
	; Copy palette itself to port 3C9h
	inc dx
PalLoop :
	; Note: Colors in a BMP file are saved as BGR values rather than RGB.
	mov al,[si+2]  ; Get red value.
	shr al,2 ; Max. is 255, but video palette maximal
	; value is 63. Therefore dividing by 4value is 63.
	out dx,al  ; Send it.
	mov al,[si+1]; Get green value.
	shr al,2 
	out dx,al ; Send it.
	mov al,[si] ; Get blue value.
	shr al,2 
	out dx,al; Send it.
	add si,4 ; Point to next color.
	; (There is a null chr. after every colorThere is a null chr. 
	loop PalLoop



	ret
endp CopyPal






proc CopyBitmap
	; BMP graphics are saved upside-down.
	; Read the graphic line by line (200 lines in VGA format),
	; displaying the lines from bottom to top.

	mov ax, 0A000h 
	mov es, ax 
	mov cx,200 
PrintBMPLoop :
	push cx
	; di = cx*320, point to the correct screen line
	mov di,cx 
	shl cx,6 
	shl di,8 
	add di,cx 
	; Read one line
	mov ah,3fh 
	mov cx,320 
	mov dx,offset ScrLine
	int 21h
	; Copy one line into video memory
	cld ; Clear direction flag, for movsb; 
	mov cx,320 
	mov si,offset ScrLine 

	rep movsb ; Copy line to the screen
	;rep movsb is same as the following coderep movsb is same as the following code 
	;mov es:di,
	;inc si
	;inc di
	;dec cx
	;loop until cx=0loop until cx=0 
	pop cx
	loop PrintBMPLoop 



	ret
endp CopyBitmap 


proc delay
    push si
	push cx
	
	mov si, 0FFFFH
od:
    mov cx, 5H
odin:
    loop odin
	dec si
	jnz od
	
	pop cx
	pop si
	ret

endp delay	

;;;;;;;;;;;;;;;;;;;;



;----------------------------------------------

proc calc_pos

	; rowGK allways row number
	; colGK allways col number
	; returns the value of posGk
	
	;(rowGK * 80 + colGK ) * 2 = posGk  
	push ax
	
	mov ax, 80d
	mul [rowGk]  ;13
	
	add ax, [colGk] ; 39d
	
	push cx
	
	mov cx, 2d 
	mul cx 
	
	pop cx
	
	mov [posGk], ax
	
	pop ax
	ret
endp calc_pos
;----------------------------


proc putGK
    
	;put the gk in di
	push ax

	mov ah, 0Ah            ; color        
	mov al, '*'              ; we'll put one star on the screen
	
	push di ; save the value of di
	mov di,[posGk]
	mov [es:di], ax        ; [es:di] - logical address; es*16 + di = 20 bit physical address	
	pop di
	
	pop ax
	
	ret
endp putGK
;------------------------
	
proc clear_star ;clears the star in di = black in di 
    push ax

	mov ah, 0              ; color        
	mov al, ' '            ; we'll put one star on the screen
	
	mov [es:di], ax        ; [es:di] - logical address; es*16 + di = 20 bit physical address	
	
	pop ax
	ret
endp clear_star
;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;---------------------------
proc setReka
	; puts the goal in the screen  
	push ax
	push bx
	push cx
	push dx
	
	mov AX, 0600h        ; AH=06(scroll up window), AL=00(entire window)
	mov BH, 11111111b    ; left nibble for background , right nibble for foreground (light gray)
	mov cx, 0048h        ; CH=00(top), CL=48(left)
	mov dx, 2580h        ; DH=25(bottom), DL=80(right)
	int 10h
	
	pop dx
	pop cx
	pop bx
	pop ax
	
	ret

endp setReka

;--------------------
proc putBall
    ; puts the random pos of the ball in the starting col 
	push ax
	push bx
	push cx
	push dx
	
	

	call rand_row;; randimze random row between 0-25 and puts it in dl 
	
	
	;call priDX

	mov bh,0
	mov bl,dl;;;

	mov [row],bx ; puts the random value in the var row

	
	
	mov [col],26h ; the number of the starting col



	mov ah, 0fh            ; color        
	mov al, '@'              ; we'll put one ball on the screen
	
	xor bh,bh
	mov bl,dl

	
	call calc_posStep ;calc the value of pos 
	push di ;save the value of di
	mov di,[pos] 
	mov [es:di], ax ;put the ball in di
	pop di
	


	pop dx
	pop cx 
	pop bx
	pop ax
	ret
endp putBall
;;;;;;;;;;;;;;;;;;;;;;;;;
proc calcTime
	push dx
	push cx
	push ax
	
	 ;total time  = (finMin*60+fin sec) - (startMin*60+startSec)
	
	mov ah,2ch ; takse the time in the end
	int 21h
	
	mov [finSec],dh
	mov [finMin],cl
	mov al,60d 
	
	mul [finMin] ;al*[finMin]. the answers is saved in ax
	add ax,[word ptr finSec]
	sub ax,[totalSec]
	mov dl,al 
	
	call printNumber ; prints ax
	
	
	pop ax
	pop cx
	pop dx
	

	ret
endp calcTime



;-----------------
proc printNumber
; Convert the number to ASCII chars and print them
	push  ax
	push  bx
	push  dx
	
	mov  bx, offset  divisorTable
	
nextDigit:
	xor ah, ah
	div  [byte ptr bx]		; al = תוצאת חילוק,  ah = שארית
	add  al, '0'
	call  printCharacter	; Display the quotient
	mov  al, ah		; ah = remainder
	add   bx, 1		; bx = address of next divisor
	cmp  [byte ptr bx], 0	; have all divisors been done?
	jne   nextDigit
	
	pop  dx
	pop  bx
	pop  ax	
	
	ret

endp printNumber
;;;;;;;;;;;;;;;;;;;;;;;;;;
proc printCharacter
; Print one char
	push ax	
	push dx
	
	mov  ah, 2
	mov dl, al
	int  21h
	
	pop  dx	
	pop  ax
	ret
endp   printCharacter

;;;;;;;;;;;;;;;;;;;;;;;;;;
proc priScore ; prints the current score in the left side of the screen 
	push di 
	push ax
	push dx
	
	mov di,320d 
	
	mov al,[count]
	add al,'0'  ;convert to ascii 
	
	mov ah,0fh ;color  = white
	
	mov [es:di],ax ;print the score in di
	
	
	pop dx
	pop ax
	pop di 


	ret
endp priScore
;-----------------------------
proc priGoal
	push ax
	push dx
	

	
	mov dx,offset msgGoal ; take the first addres of the var
	mov al,0 ;print the msg
	mov ah,9
	int 21h
	
	
	
	pop dx
	pop ax
	ret
endp priGoal	
;----------------------------
proc rand_row
; calc rand row between 0-24 = the num of possible row
	
	
	push ax
	push bx
	
	
rand:

	mov ah,2ch ;take time 
	int 21h
	mov dh,0
	and dl,00011111b ;change the num between 0-31 ; the min power of the 2 that over 25
	
	cmp dl,024d ; if the new num is over 24 rand again 
	ja rand
	

	
	
	pop bx
	pop ax
	
	
	ret
endp rand_row
;---------------------------
proc randStep
	; rands randomize num between 1-3
	push ax
	push bx
	push cx
retu:	
	mov ah,2ch
	int 21h
	and dl,0011b ;change the num between 0-3 ; the min power of the 2 that over 3
	
	cmp dl,0 ; when the rand num  = 0 rand again
	je retu
	
	
	pop cx
	pop bx
	pop ax

	ret 
endp randStep	
;---------------------
proc calc_posStep
	push bx ;take row and col and calcs it and save the answer in pos
	push ax
	
	mov ax, 80d
	mov bx,[row]
	mul bx  ;13
	
	add ax, [col] ; 39d
	
	push cx
	
	mov cx, 2d 
	mul cx
	
	pop cx
	
	mov [pos], ax

	pop ax
	pop bx
	
	ret
endp calc_posStep
;;;;;;;;;;;;;;;;;;;;;;
proc moveBall 
	
	push dx
	push cx
	push bx
	push ax

	inc [col]
	
	cmp [col],66d ;when the ball comes to the last 3 steps it comes just straight. it makes the game easier
	jae right
	
rnd:	
	call randStep
	; 1= up and rigth step 
	; 2 = right step
	; 3= down and rigth step
	
	cmp dl,1d
	je up 
	
	cmp dl,2d
	je right
	
	cmp dl,3d
	je down
	
right:
                
	jmp contin ;goes just right
	
down:
	cmp [row],24d ;check if the ball is in bottom of the screen
	je rnd ;rands again 	
	
	inc [row] ;go down
	jmp contin

up:
	cmp [row],0
	je rnd
	dec [row]
		
contin:	
	push di
	mov di,[pos]
	call clear_star ;clears the ball in di
	pop di

	
	mov ah, 0fh            ; color        
	mov al, '@'              ; we'll put the ball on the screen

	
	call calc_posStep ;calcs di with row and col 
	push di
	mov di,[pos]
	mov [es:di], ax
	pop di 


	pop ax
	pop bx
	pop cx
	pop dx



	ret 
	
	
endp moveBall
	
;-------------------------------
proc move
	
	; check the move of the gk by the keyboard
	push ax
	mov al,0
	mov ah, 1                ; check keyboard status 
    int 16h
	         ; keyboard buffer empty, we still waiting for input
	je endmoov
	
	mov ah, 0                ; we have get code in keyboard buffer, 
	                         ; we will get the scan code in <ah> register 
							; and remove this code from keyboard buffer
	int 16h
	
	cmp ah, 50h             ; is it down button?
	je godown
	
	cmp ah, 48h             ; is it up button?
	je goup

	;;;;default
	
	jmp endmoov
	
godown:
	cmp [rowGk],24d ;when the ball is in the bottom
	
	jae endmoov ;if yes, don't move!
	
	inc [rowGk]
	
	jmp endmoov	
	
goup:
	cmp [rowGk],00 ;when the ball is in the bottom
	
	je endmoov ;if yes, don't move!
	
	dec [rowGk]
		
	jmp endmoov
endmoov:
	
	pop ax 
	

	push di
	mov di,[posGk]
	call clear_star	
	pop di


	call calc_pos
	call putGK  
    call delay	
    

	
	ret
endp move
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
proc checkSave
	
	;save  = same row and colBall=col gk
	; returs 0 or 1
	
	push cx ;save the value of cx
	mov cx,[colGk] ;in order to prevent compare memory to memory
	cmp [col],cx
	pop cx

	jne saveNotOk ;when the column isnt the same column as the gk 
	
	push bx
	mov bx,[rowGk] ;in order to prevent compare memory to memory
	cmp [row],bx
	pop bx

	jne saveNotOk ;when the row isnt the same row as the gk 
	
	jmp endSaveOK ; if same row and colBall =col gk = true
	
endSaveOK: ;when the ball and the gk in the same position
	mov [saveOK],1d
	

	jmp endSave
	
saveNotOk:	
	mov [saveOK],0
	;call put_star
endSave:
	
	ret
	
endp checkSave	

; --------------------------
; Your code here 
; --------------------------	
start:  	
	mov ax, @data
	mov ds, ax

	mov ax, 0B800h         ; start address of text video memory
	                       ; 80 columns * 25 rows * 2 bytes per character:
						   ; low byte = character code; high byte = attribute (background+color)
	
	mov es, ax


	; show bmp open file
	; Graphic mode
	mov ax, 13h
	int 10h
	
	; Process BMP file
	push dx
	mov dx, offset filename 
	call printBMP
	pop dx

	; Wait for key press
	mov al,0
	mov ah,1 
	int 21h
	; Back to text mode
	mov ah, 0 
	mov al, 2 
	int 10h
	
	
	; start of the game
	 
	 
	; take time at the start 
	push dx
	push cx
	push ax
	
	mov ah,2ch
	int 21h ; take time -  cl  = min, dh = sec
	
	
	;total time  = min*60 + sec 
	
	mov [startSec],dh
	mov [startMin],cl
	mov al,60d
	
	
	
	mul [startMin]
	add al,[startSec]
	mov [totalSec],ax
	
	pop ax
	pop cx
	pop dx
	
	
	
	
	
	
	

	call setReka ;set the goal 
	mov [rowGk], 12d ; row number 
	mov [colGk], 70d ; col number
	call calc_pos  ;take the row num and col value and puts it in di
	call putGK ; put the gk in di	
	
	call putBall ;put the ball in random row in start col 
	
	call priScore
	
	
	
 ;the loop of the game 
	
game:	


	call move ;check move of the player	
	
	; clear the previous pos of the ball 
	
	push di ;save the pos of the gk  
	mov di,[pos] ;pos keeps the pos of the gk by col and row
	call clear_star ;put black in the screen 
	pop di


	call moveBall ; do one random step of the ball
	
	call checkSave ;check if three is save

	cmp [saveOK],1d ;check if the gk made the save. 1= made save,  0 = else.
	je save

	cmp [col],72d ;check if the ball is in the goal 
	je goal
	

	
	jmp game	
;;;;;;;;;;;;;;;;;;;;;;;;;

win:
	
	

	call delay  ;wating to see the msg....
	call delay
	call delay
	call delay
	call delay
	call delay
	
	; show the bmp of the end 
	; Graphic mode
	mov ax, 13h
	int 10h


	; Process BMP file
	push dx ;save the value of dx
	mov dx, offset filename2 ;the parameter 
	call printBMP
	pop dx


	; Wait for key press
	mov ah,1 
	int 21h
	; Back to text mode
	mov ah, 0 
	mov al, 2 
	int 10h
	; finish the game
	jmp exit
	
save: ;if the gk made save	
	
	push di 
	mov di,[pos]
	call clear_star
	pop di
	;call put_star
	inc [count]
	cmp [count],10d
	je win
	call priScore
	call putBall ;put the ball again in the starting col and in a random row
	
	jmp game
	
goal:
	
	call priGoal ;print the msg of the goal 
	

	call calcTime ; pint the time of the game. take the time in the end and subs it with the start time
	
	call delay ;waiting to see the msg
	call delay
	call delay
	call delay
	call delay
	call delay
	call delay
	call delay
	call delay
	call delay
	
	
	call priGoal ;pri the msg
	; show the bmp of "game over"
	; Graphic mode
	mov ax, 13h
	int 10h

	; Process BMP file
	push dx
	mov dx, offset filename1
	call printBMP
	pop dx


	; Wait for key press
	mov ah,1 
	int 21h
	; Back to text mode
	mov ah, 0 
	mov al, 2 
	int 10h
	
	
	
exit:
	mov ax, 4c00h
	int 21h
END start

