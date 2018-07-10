;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
; AddCode 例子的功能模块
;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
		.const

szErrCreate	db	'创建文件错误!',0dh,0ah,0
szMySection	db	'.adata',0
szExt		db	'_new.exe',0
szSuccess	db	'在文件后附加代码成功，新文件：',0dh,0ah
		db	'%s',0dh,0ah,0

		.code

include		_AddCode.asm

;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
; 计算按照指定值对齐后的数值
;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
_Align		proc	_dwSize,_dwAlign

		push	edx
		mov	eax,_dwSize
		xor	edx,edx
		div	_dwAlign
		.if	edx
			inc	eax
		.endif
		mul	_dwAlign
		pop	edx
		ret

_Align		endp
;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
_ProcessPeFile	proc	_lpFile,_lpPeHead,_dwSize
		local	@szNewFile[MAX_PATH]:byte
		local	@hFile,@dwTemp,@dwEntry,@lpMemory
		local	@szBuffer[256]:byte

		pushad
;********************************************************************
; （Part 1）准备工作：1－建立新文件，2－打开文件
;********************************************************************
		invoke	lstrcpy,addr @szNewFile,addr szFileName
		invoke	lstrlen,addr @szNewFile
		lea	ecx,@szNewFile
		mov	byte ptr [ecx+eax-4],0
		invoke	lstrcat,addr @szNewFile,addr szExt
		invoke	CopyFile,addr szFileName,addr @szNewFile,FALSE

		invoke	CreateFile,addr @szNewFile,GENERIC_READ or GENERIC_WRITE,FILE_SHARE_READ or \
			FILE_SHARE_WRITE,NULL,OPEN_EXISTING,FILE_ATTRIBUTE_ARCHIVE,NULL
		.if	eax ==	INVALID_HANDLE_VALUE
			invoke	SetWindowText,hWinEdit,addr szErrCreate
			jmp	_Ret
		.endif
		mov	@hFile,eax
;********************************************************************
;（Part 2）
; esi --> 原PeHead，edi --> 新的PeHead
; edx --> 最后一个节表，ebx --> 新加的节表
;********************************************************************
		mov	esi,_lpPeHead
		assume	esi:ptr IMAGE_NT_HEADERS,edi:ptr IMAGE_NT_HEADERS
		invoke	GlobalAlloc,GPTR,[esi].OptionalHeader.SizeOfHeaders
		mov	edi,eax
		invoke	RtlMoveMemory,edi,_lpFile,[esi].OptionalHeader.SizeOfHeaders
		mov	@lpMemory,eax
		mov	edi,eax
		add	edi,esi
		sub	edi,_lpFile
		movzx	eax,[esi].FileHeader.NumberOfSections
		dec	eax
		mov	ecx,sizeof IMAGE_SECTION_HEADER
		mul	ecx

		mov	edx,edi
		add	edx,eax
		add	edx,sizeof IMAGE_NT_HEADERS
		mov	ebx,edx
		add	ebx,sizeof IMAGE_SECTION_HEADER
		assume	ebx:ptr IMAGE_SECTION_HEADER,edx:ptr IMAGE_SECTION_HEADER
;********************************************************************
; （Part 3）加入一个新的节，并修正一些PE头部的内容
;********************************************************************
		inc	[edi].FileHeader.NumberOfSections
		mov	eax,[edx].PointerToRawData
		add	eax,[edx].SizeOfRawData
		mov	[ebx].PointerToRawData,eax
		invoke	_Align,offset APPEND_CODE_END-offset APPEND_CODE,[esi].OptionalHeader.FileAlignment
		mov	[ebx].SizeOfRawData,eax
		invoke	_Align,offset APPEND_CODE_END-offset APPEND_CODE,[esi].OptionalHeader.SectionAlignment
		add	[edi].OptionalHeader.SizeOfCode,eax	;修正SizeOfCode
		add	[edi].OptionalHeader.SizeOfImage,eax	;修正SizeOfImage
		invoke	_Align,[edx].Misc.VirtualSize,[esi].OptionalHeader.SectionAlignment
		add	eax,[edx].VirtualAddress
		mov	[ebx].VirtualAddress,eax
		mov	[ebx].Misc.VirtualSize,offset APPEND_CODE_END-offset APPEND_CODE
		mov	[ebx].Characteristics,IMAGE_SCN_CNT_CODE\
			or IMAGE_SCN_MEM_EXECUTE or IMAGE_SCN_MEM_READ or IMAGE_SCN_MEM_WRITE
		invoke	lstrcpy,addr [ebx].Name1,addr szMySection
;********************************************************************
; （Part 4）修正文件入口指针
;********************************************************************
		mov	eax,[ebx].VirtualAddress
		add	eax,(offset _NewEntry-offset APPEND_CODE)
		mov	[edi].OptionalHeader.AddressOfEntryPoint,eax
;********************************************************************
; （Part 5）写文件
;********************************************************************
		invoke	WriteFile,@hFile,@lpMemory,[esi].OptionalHeader.SizeOfHeaders,\
			addr @dwTemp,NULL
		invoke	SetFilePointer,@hFile,[ebx].PointerToRawData,NULL,FILE_BEGIN
		invoke	WriteFile,@hFile,offset APPEND_CODE,[ebx].Misc.VirtualSize,\
			addr @dwTemp,NULL
		mov	eax,[ebx].PointerToRawData
		add	eax,[ebx].SizeOfRawData
		invoke	SetFilePointer,@hFile,eax,NULL,FILE_BEGIN
		invoke	SetEndOfFile,@hFile
;********************************************************************
; （Part 6）修正新加代码中的 Jmp oldEntry 指令
;********************************************************************
		push	[esi].OptionalHeader.AddressOfEntryPoint
		pop	@dwEntry
		mov	eax,[ebx].VirtualAddress
		add	eax,(offset _ToOldEntry-offset APPEND_CODE+5)
		sub	@dwEntry,eax
		mov	ecx,[ebx].PointerToRawData
		add	ecx,(offset _dwOldEntry-offset APPEND_CODE)
		invoke	SetFilePointer,@hFile,ecx,NULL,FILE_BEGIN
		invoke	WriteFile,@hFile,addr @dwEntry,4,addr @dwTemp,NULL
;********************************************************************
; （Part 7）关闭文件
;********************************************************************
		invoke	GlobalFree,@lpMemory
		invoke	CloseHandle,@hFile
		invoke	wsprintf,addr @szBuffer,Addr szSuccess,addr @szNewFile
		invoke	SetWindowText,hWinEdit,addr @szBuffer
_Ret:
		assume	esi:nothing
		popad
		ret

_ProcessPeFile	endp
;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
