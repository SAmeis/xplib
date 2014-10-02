{$IFDEF HKStreamRoutines}
{$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I StFLib.inc}
{$TYPEDADDRESS OFF} //{$T-}

{ TODO -oRoger -cLIB : Remover problemas de compatibilidade }
// **********************************************************************
{$WARNINGS OFF}
// **********************************************************************

unit HKStreamRoutines;

{
  ----------------------------------------------------------------
  THKStreams v1.7 by Harry Kakoulidis 01/2002
  prog@xarka.com
  http://www.xarka.com/prog/

  This is Freeware. Please copy HKStrm17.zip unchanged.
  If you find bugs, have options etc. Please send at my e-mail.

  The use of this component is at your own risk.
  I do not take any responsibility for any damages.

  ----------------------------------------------------------------

  This unit contains code for compressing (LHA) and encrypting
  (BLOWFISH). 90% Of this file wasn't written by me, but I copyed it
  from other freeware units. I Just added code mostly to make them
  usefull for streams (The blowfish implementation needed some work;
  LHA Compression routines where ready.

  *** LHA Compression

  Source File Name              :  X2000fc.PAS (original lh5unit.pas)
  Author (Modified by)          :  Gregory L. Bullock
  The algorithm was created by  :  Haruhiko Okomura and Haruyasu Yoshizaki.

  *** Blowfish

  - designed by Bruce Schneier
  - adaption to Delphi by Steffen Kirbach and Others
  kirbach@t-online.de
  home.t-online.de/home/kirbach

}

{$DEFINE PERCOLATE}
{ .$B-,R-,S- }
{ .$A-,D-,I-,L-,O+,Q-,R-,S-,W-,Y-,Z1 }

interface

uses
  SysUtils, Classes;

procedure LHACompress(InStr, OutStr: TStream);
procedure LHAExpand(InStr, OutStr: TStream);
procedure EncryptStream(MS: TMemoryStream; const Key: string);
procedure DecryptStream(MS: TMemoryStream; const Key: string);

function Blowfish_Init(pKey: Pointer; unKeySize: Byte; pBoxes: Pointer; unRounds: Byte): Boolean;
procedure Blowfish_Done;
procedure Blowfish_ECBEncrypt(pBuffer: Pointer; unCount: Integer);
procedure Blowfish_ECBDecrypt(pBuffer: Pointer; unCount: Integer);
procedure Blowfish_CBCEncrypt(pBuffer: Pointer; unCount: Integer; var ulCBCLeft, ulCBCRight: LongInt);
procedure Blowfish_CBCDecrypt(pBuffer: Pointer; unCount: Integer; var ulCBCLeft, ulCBCRight: LongInt);
function Blowfish_GetBoxPointer: Pointer;

implementation

uses HKStreamCol;

{ *************************************************************** }

type
  TBoxes = record
	PBox: array [1 .. 34] of LongInt;
	SBox1: array [1 .. $100] of LongInt;
	SBox2: array [1 .. $100] of LongInt;
	SBox3: array [1 .. $100] of LongInt;
	SBox4: array [1 .. $100] of LongInt;
  end;

type
  T64BitArray = array [1 .. (MaxInt div 8), 0 .. 1] of LongInt;

const
  Pi_Boxes: TBoxes = (PBox: ($243F6A88, $85A308D3, $13198A2E, $03707344, $A4093822, $299F31D0, $082EFA98, $EC4E6C89, $452821E6,
	  $38D01377, $BE5466CF, $34E90C6C, $C0AC29B7, $C97C50DD, $3F84D5B5, $B5470917, $9216D5D9, $8979FB1B, $B83ACB02, $2002397A,
	  $6EC6FB5B, $FFCFD4DD, $4CBF5ED1, $F43FE582, $3EF4E823, $2D152AF0, $E718C970, $59BD9820, $1F4A9D62, $E7A529BA, $89E1248D,
	  $3BF88656, $C5114D0E, $BC4CEE16);

	  SBox1: ($D1310BA6, $98DFB5AC, $2FFD72DB, $D01ADFB7, $B8E1AFED, $6A267E96, $BA7C9045, $F12C7F99, $24A19947, $B3916CF7,
	  $0801F2E2, $858EFC16, $636920D8, $71574E69, $A458FEA3, $F4933D7E, $0D95748F, $728EB658, $718BCD58, $82154AEE, $7B54A41D,
	  $C25A59B5, $9C30D539, $2AF26013, $C5D1B023, $286085F0, $CA417918, $B8DB38EF, $8E79DCB0, $603A180E, $6C9E0E8B, $B01E8A3E,
	  $D71577C1, $BD314B27, $78AF2FDA, $55605C60, $E65525F3, $AA55AB94, $57489862, $63E81440, $55CA396A, $2AAB10B6, $B4CC5C34,
	  $1141E8CE, $A15486AF, $7C72E993, $B3EE1411, $636FBC2A, $2BA9C55D, $741831F6, $CE5C3E16, $9B87931E, $AFD6BA33, $6C24CF5C,
	  $7A325381, $28958677, $3B8F4898, $6B4BB9AF, $C4BFE81B, $66282193, $61D809CC, $FB21A991, $487CAC60, $5DEC8032, $EF845D5D,
	  $E98575B1, $DC262302, $EB651B88, $23893E81, $D396ACC5, $0F6D6FF3, $83F44239, $2E0B4482, $A4842004, $69C8F04A, $9E1F9B5E,
	  $21C66842, $F6E96C9A, $670C9C61, $ABD388F0, $6A51A0D2, $D8542F68, $960FA728, $AB5133A3, $6EEF0B6C, $137A3BE4, $BA3BF050,
	  $7EFB2A98, $A1F1651D, $39AF0176, $66CA593E, $82430E88, $8CEE8619, $456F9FB4, $7D84A5C3, $3B8B5EBE, $E06F75D8, $85C12073,
	  $401A449F, $56C16AA6, $4ED3AA62, $363F7706, $1BFEDF72, $429B023D, $37D0D724, $D00A1248, $DB0FEAD3, $49F1C09B, $075372C9,
	  $80991B7B, $25D479D8, $F6E8DEF7, $E3FE501A, $B6794C3B, $976CE0BD, $04C006BA, $C1A94FB6, $409F60C4, $5E5C9EC2, $196A2463,
	  $68FB6FAF, $3E6C53B5, $1339B2EB, $3B52EC6F, $6DFC511F, $9B30952C, $CC814544, $AF5EBD09, $BEE3D004, $DE334AFD, $660F2807,
	  $192E4BB3, $C0CBA857, $45C8740F, $D20B5F39, $B9D3FBDB, $5579C0BD, $1A60320A, $D6A100C6, $402C7279, $679F25FE, $FB1FA3CC,
	  $8EA5E9F8, $DB3222F8, $3C7516DF, $FD616B15, $2F501EC8, $AD0552AB, $323DB5FA, $FD238760, $53317B48, $3E00DF82, $9E5C57BB,
	  $CA6F8CA0, $1A87562E, $DF1769DB, $D542A8F6, $287EFFC3, $AC6732C6, $8C4F5573, $695B27B0, $BBCA58C8, $E1FFA35D, $B8F011A0,
	  $10FA3D98, $FD2183B8, $4AFCB56C, $2DD1D35B, $9A53E479, $B6F84565, $D28E49BC, $4BFB9790, $E1DDF2DA, $A4CB7E33, $62FB1341,
	  $CEE4C6E8, $EF20CADA, $36774C01, $D07E9EFE, $2BF11FB4, $95DBDA4D, $AE909198, $EAAD8E71, $6B93D5A0, $D08ED1D0, $AFC725E0,
	  $8E3C5B2F, $8E7594B7, $8FF6E2FB, $F2122B64, $8888B812, $900DF01C, $4FAD5EA0, $688FC31C, $D1CFF191, $B3A8C1AD, $2F2F2218,
	  $BE0E1777, $EA752DFE, $8B021FA1, $E5A0CC0F, $B56F74E8, $18ACF3D6, $CE89E299, $B4A84FE0, $FD13E0B7, $7CC43B81, $D2ADA8D9,
	  $165FA266, $80957705, $93CC7314, $211A1477, $E6AD2065, $77B5FA86, $C75442F5, $FB9D35CF, $EBCDAF0C, $7B3E89A0, $D6411BD3,
	  $AE1E7E49, $00250E2D, $2071B35E, $226800BB, $57B8E0AF, $2464369B, $F009B91E, $5563911D, $59DFA6AA, $78C14389, $D95A537F,
	  $207D5BA2, $02E5B9C5, $83260376, $6295CFA9, $11C81968, $4E734A41, $B3472DCA, $7B14A94A, $1B510052, $9A532915, $D60F573F,
	  $BC9BC6E4, $2B60A476, $81E67400, $08BA6FB5, $571BE91F, $F296EC6B, $2A0DD915, $B6636521, $E7B9F9B6, $FF34052E, $C5855664,
	  $53B02D5D, $A99F8FA1, $08BA4799, $6E85076A);

	  SBox2: ($4B7A70E9, $B5B32944, $DB75092E, $C4192623, $AD6EA6B0, $49A7DF7D, $9CEE60B8, $8FEDB266, $ECAA8C71, $699A17FF,
	  $5664526C, $C2B19EE1, $193602A5, $75094C29, $A0591340, $E4183A3E, $3F54989A, $5B429D65, $6B8FE4D6, $99F73FD6, $A1D29C07,
	  $EFE830F5, $4D2D38E6, $F0255DC1, $4CDD2086, $8470EB26, $6382E9C6, $021ECC5E, $09686B3F, $3EBAEFC9, $3C971814, $6B6A70A1,
	  $687F3584, $52A0E286, $B79C5305, $AA500737, $3E07841C, $7FDEAE5C, $8E7D44EC, $5716F2B8, $B03ADA37, $F0500C0D, $F01C1F04,
	  $0200B3FF, $AE0CF51A, $3CB574B2, $25837A58, $DC0921BD, $D19113F9, $7CA92FF6, $94324773, $22F54701, $3AE5E581, $37C2DADC,
	  $C8B57634, $9AF3DDA7, $A9446146, $0FD0030E, $ECC8C73E, $A4751E41, $E238CD99, $3BEA0E2F, $3280BBA1, $183EB331, $4E548B38,
	  $4F6DB908, $6F420D03, $F60A04BF, $2CB81290, $24977C79, $5679B072, $BCAF89AF, $DE9A771F, $D9930810, $B38BAE12, $DCCF3F2E,
	  $5512721F, $2E6B7124, $501ADDE6, $9F84CD87, $7A584718, $7408DA17, $BC9F9ABC, $E94B7D8C, $EC7AEC3A, $DB851DFA, $63094366,
	  $C464C3D2, $EF1C1847, $3215D908, $DD433B37, $24C2BA16, $12A14D43, $2A65C451, $50940002, $133AE4DD, $71DFF89E, $10314E55,
	  $81AC77D6, $5F11199B, $043556F1, $D7A3C76B, $3C11183B, $5924A509, $F28FE6ED, $97F1FBFA, $9EBABF2C, $1E153C6E, $86E34570,
	  $EAE96FB1, $860E5E0A, $5A3E2AB3, $771FE71C, $4E3D06FA, $2965DCB9, $99E71D0F, $803E89D6, $5266C825, $2E4CC978, $9C10B36A,
	  $C6150EBA, $94E2EA78, $A5FC3C53, $1E0A2DF4, $F2F74EA7, $361D2B3D, $1939260F, $19C27960, $5223A708, $F71312B6, $EBADFE6E,
	  $EAC31F66, $E3BC4595, $A67BC883, $B17F37D1, $018CFF28, $C332DDEF, $BE6C5AA5, $65582185, $68AB9802, $EECEA50F, $DB2F953B,
	  $2AEF7DAD, $5B6E2F84, $1521B628, $29076170, $ECDD4775, $619F1510, $13CCA830, $EB61BD96, $0334FE1E, $AA0363CF, $B5735C90,
	  $4C70A239, $D59E9E0B, $CBAADE14, $EECC86BC, $60622CA7, $9CAB5CAB, $B2F3846E, $648B1EAF, $19BDF0CA, $A02369B9, $655ABB50,
	  $40685A32, $3C2AB4B3, $319EE9D5, $C021B8F7, $9B540B19, $875FA099, $95F7997E, $623D7DA8, $F837889A, $97E32D77, $11ED935F,
	  $16681281, $0E358829, $C7E61FD6, $96DEDFA1, $7858BA99, $57F584A5, $1B227263, $9B83C3FF, $1AC24696, $CDB30AEB, $532E3054,
	  $8FD948E4, $6DBC3128, $58EBF2EF, $34C6FFEA, $FE28ED61, $EE7C3C73, $5D4A14D9, $E864B7E3, $42105D14, $203E13E0, $45EEE2B6,
	  $A3AAABEA, $DB6C4F15, $FACB4FD0, $C742F442, $EF6ABBB5, $654F3B1D, $41CD2105, $D81E799E, $86854DC7, $E44B476A, $3D816250,
	  $CF62A1F2, $5B8D2646, $FC8883A0, $C1C7B6A3, $7F1524C3, $69CB7492, $47848A0B, $5692B285, $095BBF00, $AD19489D, $1462B174,
	  $23820E00, $58428D2A, $0C55F5EA, $1DADF43E, $233F7061, $3372F092, $8D937E41, $D65FECF1, $6C223BDB, $7CDE3759, $CBEE7460,
	  $4085F2A7, $CE77326E, $A6078084, $19F8509E, $E8EFD855, $61D99735, $A969A7AA, $C50C06C2, $5A04ABFC, $800BCADC, $9E447A2E,
	  $C3453484, $FDD56705, $0E1E9EC9, $DB73DBD3, $105588CD, $675FDA79, $E3674340, $C5C43465, $713E38D8, $3D28F89E, $F16DFF20,
	  $153E21E7, $8FB03D4A, $E6E39F2B, $DB83ADF7);

	  SBox3: ($E93D5A68, $948140F7, $F64C261C, $94692934, $411520F7, $7602D4F7, $BCF46B2E, $D4A20068, $D4082471, $3320F46A,
	  $43B7D4B7, $500061AF, $1E39F62E, $97244546, $14214F74, $BF8B8840, $4D95FC1D, $96B591AF, $70F4DDD3, $66A02F45, $BFBC09EC,
	  $03BD9785, $7FAC6DD0, $31CB8504, $96EB27B3, $55FD3941, $DA2547E6, $ABCA0A9A, $28507825, $530429F4, $0A2C86DA, $E9B66DFB,
	  $68DC1462, $D7486900, $680EC0A4, $27A18DEE, $4F3FFEA2, $E887AD8C, $B58CE006, $7AF4D6B6, $AACE1E7C, $D3375FEC, $CE78A399,
	  $406B2A42, $20FE9E35, $D9F385B9, $EE39D7AB, $3B124E8B, $1DC9FAF7, $4B6D1856, $26A36631, $EAE397B2, $3A6EFA74, $DD5B4332,
	  $6841E7F7, $CA7820FB, $FB0AF54E, $D8FEB397, $454056AC, $BA489527, $55533A3A, $20838D87, $FE6BA9B7, $D096954B, $55A867BC,
	  $A1159A58, $CCA92963, $99E1DB33, $A62A4A56, $3F3125F9, $5EF47E1C, $9029317C, $FDF8E802, $04272F70, $80BB155C, $05282CE3,
	  $95C11548, $E4C66D22, $48C1133F, $C70F86DC, $07F9C9EE, $41041F0F, $404779A4, $5D886E17, $325F51EB, $D59BC0D1, $F2BCC18F,
	  $41113564, $257B7834, $602A9C60, $DFF8E8A3, $1F636C1B, $0E12B4C2, $02E1329E, $AF664FD1, $CAD18115, $6B2395E0, $333E92E1,
	  $3B240B62, $EEBEB922, $85B2A20E, $E6BA0D99, $DE720C8C, $2DA2F728, $D0127845, $95B794FD, $647D0862, $E7CCF5F0, $5449A36F,
	  $877D48FA, $C39DFD27, $F33E8D1E, $0A476341, $992EFF74, $3A6F6EAB, $F4F8FD37, $A812DC60, $A1EBDDF8, $991BE14C, $DB6E6B0D,
	  $C67B5510, $6D672C37, $2765D43B, $DCD0E804, $F1290DC7, $CC00FFA3, $B5390F92, $690FED0B, $667B9FFB, $CEDB7D9C, $A091CF0B,
	  $D9155EA3, $BB132F88, $515BAD24, $7B9479BF, $763BD6EB, $37392EB3, $CC115979, $8026E297, $F42E312D, $6842ADA7, $C66A2B3B,
	  $12754CCC, $782EF11C, $6A124237, $B79251E7, $06A1BBE6, $4BFB6350, $1A6B1018, $11CAEDFA, $3D25BDD8, $E2E1C3C9, $44421659,
	  $0A121386, $D90CEC6E, $D5ABEA2A, $64AF674E, $DA86A85F, $BEBFE988, $64E4C3FE, $9DBC8057, $F0F7C086, $60787BF8, $6003604D,
	  $D1FD8346, $F6381FB0, $7745AE04, $D736FCCC, $83426B33, $F01EAB71, $B0804187, $3C005E5F, $77A057BE, $BDE8AE24, $55464299,
	  $BF582E61, $4E58F48F, $F2DDFDA2, $F474EF38, $8789BDC2, $5366F9C3, $C8B38E74, $B475F255, $46FCD9B9, $7AEB2661, $8B1DDF84,
	  $846A0E79, $915F95E2, $466E598E, $20B45770, $8CD55591, $C902DE4C, $B90BACE1, $BB8205D0, $11A86248, $7574A99E, $B77F19B6,
	  $E0A9DC09, $662D09A1, $C4324633, $E85A1F02, $09F0BE8C, $4A99A025, $1D6EFE10, $1AB93D1D, $0BA5A4DF, $A186F20F, $2868F169,
	  $DCB7DA83, $573906FE, $A1E2CE9B, $4FCD7F52, $50115E01, $A70683FA, $A002B5C4, $0DE6D027, $9AF88C27, $773F8641, $C3604C06,
	  $61A806B5, $F0177A28, $C0F586E0, $006058AA, $30DC7D62, $11E69ED7, $2338EA63, $53C2DD94, $C2C21634, $BBCBEE56, $90BCB6DE,
	  $EBFC7DA1, $CE591D76, $6F05E409, $4B7C0188, $39720A3D, $7C927C24, $86E3725F, $724D9DB9, $1AC15BB4, $D39EB8FC, $ED545578,
	  $08FCA5B5, $D83D7CD3, $4DAD0FC4, $1E50EF5E, $B161E6F8, $A28514D9, $6C51133C, $6FD5C7E7, $56E14EC4, $362ABFCE, $DDC6C837,
	  $D79A3234, $92638212, $670EFA8E, $406000E0);

	  SBox4: ($3A39CE37, $D3FAF5CF, $ABC27737, $5AC52D1B, $5CB0679E, $4FA33742, $D3822740, $99BC9BBE, $D5118E9D, $BF0F7315,
	  $D62D1C7E, $C700C47B, $B78C1B6B, $21A19045, $B26EB1BE, $6A366EB4, $5748AB2F, $BC946E79, $C6A376D2, $6549C2C8, $530FF8EE,
	  $468DDE7D, $D5730A1D, $4CD04DC6, $2939BBDB, $A9BA4650, $AC9526E8, $BE5EE304, $A1FAD5F0, $6A2D519A, $63EF8CE2, $9A86EE22,
	  $C089C2B8, $43242EF6, $A51E03AA, $9CF2D0A4, $83C061BA, $9BE96A4D, $8FE51550, $BA645BD6, $2826A2F9, $A73A3AE1, $4BA99586,
	  $EF5562E9, $C72FEFD3, $F752F7DA, $3F046F69, $77FA0A59, $80E4A915, $87B08601, $9B09E6AD, $3B3EE593, $E990FD5A, $9E34D797,
	  $2CF0B7D9, $022B8B51, $96D5AC3A, $017DA67D, $D1CF3ED6, $7C7D2D28, $1F9F25CF, $ADF2B89B, $5AD6B472, $5A88F54C, $E029AC71,
	  $E019A5E6, $47B0ACFD, $ED93FA9B, $E8D3C48D, $283B57CC, $F8D56629, $79132E28, $785F0191, $ED756055, $F7960E44, $E3D35E8C,
	  $15056DD4, $88F46DBA, $03A16125, $0564F0BD, $C3EB9E15, $3C9057A2, $97271AEC, $A93A072A, $1B3F6D9B, $1E6321F5, $F59C66FB,
	  $26DCF319, $7533D928, $B155FDF5, $03563482, $8ABA3CBB, $28517711, $C20AD9F8, $ABCC5167, $CCAD925F, $4DE81751, $3830DC8E,
	  $379D5862, $9320F991, $EA7A90C2, $FB3E7BCE, $5121CE64, $774FBE32, $A8B6E37E, $C3293D46, $48DE5369, $6413E680, $A2AE0810,
	  $DD6DB224, $69852DFD, $09072166, $B39A460A, $6445C0DD, $586CDECF, $1C20C8AE, $5BBEF7DD, $1B588D40, $CCD2017F, $6BB4E3BB,
	  $DDA26A7E, $3A59FF45, $3E350A44, $BCB4CDD5, $72EACEA8, $FA6484BB, $8D6612AE, $BF3C6F47, $D29BE463, $542F5D9E, $AEC2771B,
	  $F64E6370, $740E0D8D, $E75B1357, $F8721671, $AF537D5D, $4040CB08, $4EB4E2CC, $34D2466A, $0115AF84, $E1B00428, $95983A1D,
	  $06B89FB4, $CE6EA048, $6F3F3B82, $3520AB82, $011A1D4B, $277227F8, $611560B1, $E7933FDC, $BB3A792B, $344525BD, $A08839E1,
	  $51CE794B, $2F32C9B7, $A01FBAC9, $E01CC87E, $BCC7D1F6, $CF0111C3, $A1E8AAC7, $1A908749, $D44FBD9A, $D0DADECB, $D50ADA38,
	  $0339C32A, $C6913667, $8DF9317C, $E0B12B4F, $F79E59B7, $43F5BB3A, $F2D519FF, $27D9459C, $BF97222C, $15E6FC2A, $0F91FC71,
	  $9B941525, $FAE59361, $CEB69CEB, $C2A86459, $12BAA8D1, $B6C1075E, $E3056A0C, $10D25065, $CB03A442, $E0EC6E0E, $1698DB3B,
	  $4C98A0BE, $3278E964, $9F1F9532, $E0D392DF, $D3A0342B, $8971F21E, $1B0A7441, $4BA3348C, $C5BE7120, $C37632D8, $DF359F8D,
	  $9B992F2E, $E60B6F47, $0FE3F11D, $E54CDA54, $1EDAD891, $CE6279CF, $CD3E7E6F, $1618B166, $FD2C1D05, $848FD2C5, $F6FB2299,
	  $F523F357, $A6327623, $93A83531, $56CCCD02, $ACF08162, $5A75EBB5, $6E163697, $88D273CC, $DE966292, $81B949D0, $4C50901B,
	  $71C65614, $E6C6C7BD, $327A140A, $45E1D006, $C3F27B9A, $C9AA53FD, $62A80F00, $BB25BFE2, $35BDD2F6, $71126905, $B2040222,
	  $B6CBCF7C, $CD769C2B, $53113EC0, $1640E3D3, $38ABBD60, $2547ADF0, $BA38209C, $F746CE76, $77AFA1C5, $20756060, $85CBFE4E,
	  $8AE88DD8, $7AAAF9B0, $4CF9AA7E, $1948C25C, $02FB8A8C, $01C36AE4, $D6EBE1F9, $90D4F869, $A65CDEA0, $3F09252D, $C208E69F,
	  $B74E6132, $CE77E25B, $578FDFE3, $3AC372E6));

var
  Boxes: TBoxes;
  Rounds: LongInt;

procedure BLOWFISH_ENCIPHER(var XR, XL: LongInt);
asm
  push    ebx
  push    ecx
  push    esi
  push    edi
  push  eax
  push  edx
  mov   eax, [XR]
  mov   edx, [XL]
  mov   ebx, 0
  mov   esi, 0
  mov    edi, rounds
  shl    edi, 2
@:xor   edx, [esi*1   + offset boxes.pbox]
  ror    edx, 16
  mov    bl, dh
  mov    ecx, [ebx*4 + offset boxes.sbox1]
  mov    bl, dl
  add    ecx, [ebx*4 + offset boxes.sbox2]
  rol    edx, 16
  mov   bl, dh
  xor   ecx, [ebx*4 + offset boxes.sbox3]
  mov   bl, dl
  add   ecx, [ebx*4 + offset boxes.sbox4]
  xor    eax, ecx
  xchg    eax, edx
  add    esi, 4
  cmp    esi, edi
  jne    @
  xchg  eax, edx
  xor   eax, [esi*1   + offset boxes.pbox]
  xor   edx, [esi+4 + offset boxes.pbox]
  mov   ebx, edx
  pop   edx
  mov[XL], ebx
  mov   ebx, eax
  pop   eax
  mov[XR], ebx
  pop    edi
  pop    esi
  pop    ecx
  pop    ebx
  ret
end;

procedure BLOWFISH_DECIPHER(var XR, XL: LongInt);
asm
  push    ebx
  push    ecx
  push    esi
  push  eax
  push  edx
  mov   eax, [XR]
  mov   edx, [XL]
  mov   ebx, 0
  mov    esi, rounds
  inc   esi
  shl    esi, 2
@:xor   edx, [esi*1   + offset boxes.pbox]
  ror    edx, 16
  mov    bl, dh
  mov    ecx, [ebx*4 + offset boxes.sbox1]
  mov    bl, dl
  add    ecx, [ebx*4 + offset boxes.sbox2]
  rol    edx, 16
  mov   bl, dh
  xor   ecx, [ebx*4 + offset boxes.sbox3]
  mov   bl, dl
  add   ecx, [ebx*4 + offset boxes.sbox4]
  xor    eax, ecx
  xchg    eax, edx
  sub    esi, 4
  cmp    esi, 4
  jne    @
  xchg  eax, edx
  xor   eax, [4 + offset boxes.pbox]
  xor   edx, [offset boxes.pbox]
  mov   ebx, edx
  pop   edx
  mov[XL], ebx
  mov   ebx, eax
  pop   eax
  mov[XR], ebx
  pop    esi
  pop    ecx
  pop    ebx
  ret
end;

function Blowfish_Init;
type
  TpKey = array [0 .. 53] of Byte;
  T_X   = array [Pred(low(Boxes.PBox)) .. Pred(high(Boxes.PBox)), 0 .. 3] of Byte;
  TPBox = array [low(Boxes.PBox) .. high(Boxes.PBox) div 2, 0 .. 1] of LongInt;
  TSBox = array [low(Boxes.SBox1) .. high(Boxes.SBox1) div 2, 0 .. 1] of LongInt;
  T_Box = array [low(Boxes.SBox1) .. high(Boxes.SBox1)] of LongInt;
var
  i, j: Integer;
  XL, XR: LongInt;
begin
  Result := FALSE;

  if Assigned(pBoxes) then begin
	Boxes := TBoxes(pBoxes^);
  end else begin
	Boxes := Pi_Boxes;
  end;

  if unRounds in [2 .. 32] then begin
	Rounds := unRounds + unRounds mod 2;
  end else begin
	Rounds := 16;
  end;

  for i := low(T_X) to Pred((Rounds + 2) * 4) do begin
	T_X(Boxes.PBox)[i div 4, 3 - (i mod 4)] := T_X(Boxes.PBox)[i div 4, 3 - (i mod 4)] xor TpKey(pKey^)[i mod unKeySize];
  end;

  XL := 0;
  XR := 0;

  for i := low(TPBox) to ((Rounds + 2) div 2) do begin
	BLOWFISH_ENCIPHER(XR, XL);
	TPBox(Boxes.PBox)[i, 0] := XL;
	TPBox(Boxes.PBox)[i, 1] := XR;
  end;

  for i := low(TSBox) to high(TSBox) do begin
	BLOWFISH_ENCIPHER(XR, XL);
	TSBox(Boxes.SBox1)[i, 0] := XL;
	TSBox(Boxes.SBox1)[i, 1] := XR;
  end;

  for i := low(TSBox) to high(TSBox) do begin
	BLOWFISH_ENCIPHER(XR, XL);
	TSBox(Boxes.SBox2)[i, 0] := XL;
	TSBox(Boxes.SBox2)[i, 1] := XR;
  end;

  for i := low(TSBox) to high(TSBox) do begin
	BLOWFISH_ENCIPHER(XR, XL);
	TSBox(Boxes.SBox3)[i, 0] := XL;
	TSBox(Boxes.SBox3)[i, 1] := XR;
  end;

  for i := low(TSBox) to high(TSBox) do begin
	BLOWFISH_ENCIPHER(XR, XL);
	TSBox(Boxes.SBox4)[i, 0] := XL;
	TSBox(Boxes.SBox4)[i, 1] := XR;
  end;

  for i := low(T_Box) to high(T_Box) do begin
	for j := Succ(i) to high(T_Box) do begin
	  if T_Box(Boxes.SBox1)[i] = T_Box(Boxes.SBox1)[j] then begin
		Exit;
	  end;
	end;
  end;

  for i := low(T_Box) to high(T_Box) do begin
	for j := Succ(i) to high(T_Box) do begin
	  if T_Box(Boxes.SBox2)[i] = T_Box(Boxes.SBox2)[j] then begin
		Exit;
	  end;
	end;
  end;

  for i := low(T_Box) to high(T_Box) do begin
	for j := Succ(i) to high(T_Box) do begin
	  if T_Box(Boxes.SBox3)[i] = T_Box(Boxes.SBox3)[j] then begin
		Exit;
	  end;
	end;
  end;

  for i := low(T_Box) to high(T_Box) do begin
	for j := Succ(i) to high(T_Box) do begin
	  if T_Box(Boxes.SBox4)[i] = T_Box(Boxes.SBox4)[j] then begin
		Exit;
	  end;
	end;
  end;

  Result := TRUE;
end;

procedure Blowfish_Done;
begin
  Rounds := 16;
  Boxes := Pi_Boxes;
end;

procedure Blowfish_ECBEncrypt;
var
  i: Integer;
begin
  for i := 1 to (unCount div 8) do begin
	BLOWFISH_ENCIPHER(T64BitArray(pBuffer^)[i, 1], T64BitArray(pBuffer^)[i, 0]);
  end;
end;

procedure Blowfish_ECBDecrypt;
var
  i: Integer;
begin
  for i := 1 to (unCount div 8) do begin
	BLOWFISH_DECIPHER(T64BitArray(pBuffer^)[i, 1], T64BitArray(pBuffer^)[i, 0]);
  end;
end;

procedure Blowfish_CBCEncrypt;
var
  i: Integer;
begin
  for i := 1 to (unCount div 8) do begin
	ulCBCLeft := T64BitArray(pBuffer^)[i, 0] xor ulCBCLeft;
	ulCBCRight := T64BitArray(pBuffer^)[i, 1] xor ulCBCRight;
	BLOWFISH_ENCIPHER(ulCBCRight, ulCBCLeft);
	T64BitArray(pBuffer^)[i, 0] := ulCBCLeft;
	T64BitArray(pBuffer^)[i, 1] := ulCBCRight;
  end;
end;

procedure Blowfish_CBCDecrypt;
var
  i: Integer;
  XL, XR: LongInt;
begin
  for i := 1 to (unCount div 8) do begin
	XL := T64BitArray(pBuffer^)[i, 0];
	XR := T64BitArray(pBuffer^)[i, 1];
	BLOWFISH_DECIPHER(T64BitArray(pBuffer^)[i, 1], T64BitArray(pBuffer^)[i, 0]);
	T64BitArray(pBuffer^)[i, 0] := T64BitArray(pBuffer^)[i, 0] xor ulCBCLeft;
	T64BitArray(pBuffer^)[i, 1] := T64BitArray(pBuffer^)[i, 1] xor ulCBCRight;
	ulCBCLeft := XL;
	ulCBCRight := XR;
  end;
end;

function Blowfish_GetBoxPointer;
begin
  Result := @Boxes;
end;

function SetKeyword(Value: string): Boolean;
begin
  Result := Blowfish_Init(PChar(Value), Length(Value), nil, 16);
end;

procedure AddPadding(MS: TStream);
const
  Pad: array [1 .. 8] of char = #1#1#1#1#1#1#1#1;
var
  num: Byte;
begin
  num := 8 - ((MS.size + 1) mod 8);
  MS.Position := MS.size;
  if num > 0 then begin
	MS.Write(Pad, num);
  end;
  MS.Write(num, sizeof(num));
end;

procedure EncryptStream(MS: TMemoryStream; const Key: string);
begin
  if MS.size > 0 then begin
	if SetKeyword(Key) then begin
	  AddPadding(MS);
	  Blowfish_ECBEncrypt(MS.memory, MS.size);
	end;
  end;
end;

procedure DecryptStream(MS: TMemoryStream; const Key: string);
var
  num: Byte;
begin
  if MS.size > 0 then begin
	if SetKeyword(Key) then begin
	  Blowfish_ECBDecrypt(MS.memory, MS.size);
	  MS.Position := MS.size - 1;
	  MS.read(num, sizeof(num));
	  MS.SetSize(MS.size - num - 1);
	end;
  end;
end;

{ *************************************************************** }
{ *************************************************************** }
{ *************************************************************** }
{ *************************************************************** }
{ LZH Rourines }
{ *************************************************************** }
{ *************************************************************** }
{ *************************************************************** }
{ *************************************************************** }
{ *************************************************************** }

type
  {$IFDEF WIN32}
  TwoByteInt = SmallInt;
  {$ELSE}
  TwoByteInt = Integer;
  {$ENDIF}
  PWord = ^TWord;
  TWord = array [0 .. 32759] of TwoByteInt;
  PByte = ^TByte;
  TByte = array [0 .. 65519] of Byte;

const

  BITBUFSIZ = 16;
  UCHARMAX  = 255;

  DICBIT = 13;
  DICSIZ = 1 shl DICBIT;

  MATCHBIT  = 8;
  MAXMATCH  = 1 shl MATCHBIT;
  THRESHOLD = 3;
  PERCFLAG  = $8000;

  NC      = (UCHARMAX + MAXMATCH + 2 - THRESHOLD);
  CBIT    = 9;
  CODEBIT = 16;

  NP   = DICBIT + 1;
  NT   = CODEBIT + 3;
  PBIT = 4; { Log2(NP) }
  TBIT = 5; { Log2(NT) }
  NPT  = NT; { Greater from NP and NT }

  NUL        = 0;
  MAXHASHVAL = (3 * DICSIZ + (DICSIZ shr 9 + 1) * UCHARMAX);

  WINBIT     = 14;
  WINDOWSIZE = 1 shl WINBIT;

  BUFBIT  = 13;
  BUFSIZE = 1 shl BUFBIT;

type
  BufferArray    = array [0 .. Pred(BUFSIZE)] of Byte;
  LeftRightArray = array [0 .. 2 * (NC - 1)] of Word;
  CTableArray    = array [0 .. 4095] of Word;
  CLenArray      = array [0 .. Pred(NC)] of Byte;
  HeapArray      = array [0 .. NC] of Word;

var
  OrigSize, CompSize: LongInt;
  InFile, OutFile: TStream;

  BitBuf: Word;
  n, HeapSize: TwoByteInt;
  SubBitBuf, BitCount: Word;

  Buffer: ^BufferArray;
  BufPtr: Word;

  Left, Right: ^LeftRightArray;

  PtTable: array [0 .. 255] of Word;
  PtLen: array [0 .. Pred(NPT)] of Byte;
  CTable: ^CTableArray;
  CLen: ^CLenArray;

  BlockSize: Word;

  { The following variables are used by the compression engine only }

  Heap: ^HeapArray;
  LenCnt: array [0 .. 16] of Word;

  Freq, SortPtr: PWord;
  Len: PByte;
  Depth: Word;

  Buf: PByte;

  CFreq: array [0 .. 2 * (NC - 1)] of Word;
  PFreq: array [0 .. 2 * (NP - 1)] of Word;
  TFreq: array [0 .. 2 * (NT - 1)] of Word;

  CCode: array [0 .. Pred(NC)] of Word;
  PtCode: array [0 .. Pred(NPT)] of Word;

  CPos, OutputPos, OutputMask: Word;
  Text, ChildCount: PByte;

  Pos, MatchPos, Avail: Word;
  Position, Parent, Prev, Next: PWord;

  Remainder, MatchLen: TwoByteInt;
  Level: PByte;

  { ********************************** File I/O ********************************** }

function GetC: Byte;
begin
  if BufPtr = 0 then begin
	InFile.read(Buffer^, BUFSIZE);
  end;
  GetC := Buffer^[BufPtr];
  BufPtr := Succ(BufPtr) and Pred(BUFSIZE);
end;

procedure PutC(c: Byte);
begin
  if BufPtr = BUFSIZE then begin
	OutFile.Write(Buffer^, BUFSIZE);
	BufPtr := 0;
  end;
  Buffer^[BufPtr] := c;
  inc(BufPtr);
end;

function BRead(p: Pointer; n: TwoByteInt): TwoByteInt;
begin
  BRead := InFile.read(p^, n);
end;

procedure BWrite(p: Pointer; n: TwoByteInt);
begin
  OutFile.Write(p^, n);
end;

{ **************************** Bit handling routines *************************** }

procedure FillBuf(n: TwoByteInt);
begin
  BitBuf := (BitBuf shl n);
  while n > BitCount do begin
	DEC(n, BitCount);
	BitBuf := BitBuf or (SubBitBuf shl n);
	if (CompSize <> 0) then begin
	  DEC(CompSize);
	  SubBitBuf := GetC;
	end else begin
	  SubBitBuf := 0;
	end;
	BitCount := 8;
  end;
  DEC(BitCount, n);
  BitBuf := BitBuf or (SubBitBuf shr BitCount);
end;

function GetBits(n: TwoByteInt): Word;
begin
  GetBits := BitBuf shr (BITBUFSIZ - n);
  FillBuf(n);
end;

procedure PutBits(n: TwoByteInt; x: Word);
begin
  if n < BitCount then begin
	DEC(BitCount, n);
	SubBitBuf := SubBitBuf or (x shl BitCount);
  end else begin
	DEC(n, BitCount);
	PutC(SubBitBuf or (x shr n));
	inc(CompSize);
	if n < 8 then begin
	  BitCount := 8 - n;
	  SubBitBuf := x shl BitCount;
	end else begin
	  PutC(x shr (n - 8));
	  inc(CompSize);
	  BitCount := 16 - n;
	  SubBitBuf := x shl BitCount;
	end;
  end;
end;

procedure InitGetBits;
begin
  BitBuf := 0;
  SubBitBuf := 0;
  BitCount := 0;
  FillBuf(BITBUFSIZ);
end;

procedure InitPutBits;
begin
  BitCount := 8;
  SubBitBuf := 0;
end;

{ ******************************** Decompression ******************************* }

procedure MakeTable(nchar: TwoByteInt; BitLen: PByte; TableBits: TwoByteInt; Table: PWord);
var
  count, weight: array [1 .. 16] of Word;
  start: array [1 .. 17] of Word;
  p: PWord;
  i, k, Len, ch, jutbits, Avail, nextCode, mask: TwoByteInt;
begin
  for i := 1 to 16 do begin
	count[i] := 0;
  end;
  for i := 0 to Pred(nchar) do begin
	inc(count[BitLen^[i]]);
  end;
  start[1] := 0;
  for i := 1 to 16 do begin
	start[Succ(i)] := start[i] + (count[i] shl (16 - i));
  end;
  if start[17] <> 0 then begin
	raise ECorruptFile.Create('Compressed file is corrupt');
	Exit;
  end;
  jutbits := 16 - TableBits;
  for i := 1 to TableBits do begin
	start[i] := start[i] shr jutbits;
	weight[i] := 1 shl (TableBits - i);
  end;
  i := Succ(TableBits);
  while (i <= 16) do begin
	weight[i] := 1 shl (16 - i);
	inc(i);
  end;
  i := start[Succ(TableBits)] shr jutbits;
  if i <> 0 then begin
	k := 1 shl TableBits;
	while i <> k do begin
	  Table^[i] := 0;
	  inc(i);
	end;
  end;
  Avail := nchar;
  mask := 1 shl (15 - TableBits);
  for ch := 0 to Pred(nchar) do begin
	Len := BitLen^[ch];
	if Len = 0 then begin
	  CONTINUE;
	end;
	k := start[Len];
	nextCode := k + weight[Len];
	if Len <= TableBits then begin
	  for i := k to Pred(nextCode) do begin
		Table^[i] := ch;
	  end;
	end else begin
	  p := Addr(Table^[Word(k) shr jutbits]);
	  i := Len - TableBits;
	  while i <> 0 do begin
		if p^[0] = 0 then begin
		  Right^[Avail] := 0;
		  Left^[Avail] := 0;
		  p^[0] := Avail;
		  inc(Avail);
		end;
		if (k and mask) <> 0 then begin
		  p := Addr(Right^[p^[0]]);
		end else begin
		  p := Addr(Left^[p^[0]]);
		end;
		k := k shl 1;
		DEC(i);
	  end;
	  p^[0] := ch;
	end;
	start[Len] := nextCode;
  end;
end;

procedure ReadPtLen(nn, nBit, ispecial: TwoByteInt);
var
  i, c, n: TwoByteInt;
  mask: Word;
begin
  n := GetBits(nBit);
  if n = 0 then begin
	c := GetBits(nBit);
	for i := 0 to Pred(nn) do begin
	  PtLen[i] := 0;
	end;
	for i := 0 to 255 do begin
	  PtTable[i] := c;
	end;
  end else begin
	i := 0;
	while (i < n) do begin
	  c := BitBuf shr (BITBUFSIZ - 3);
	  if c = 7 then begin
		mask := 1 shl (BITBUFSIZ - 4);
		while (mask and BitBuf) <> 0 do begin
		  mask := mask shr 1;
		  inc(c);
		end;
	  end;
	  if c < 7 then begin
		FillBuf(3);
	  end else begin
		FillBuf(c - 3);
	  end;
	  PtLen[i] := c;
	  inc(i);
	  if i = ispecial then begin
		c := Pred(TwoByteInt(GetBits(2)));
		while c >= 0 do begin
		  PtLen[i] := 0;
		  inc(i);
		  DEC(c);
		end;
	  end;
	end;
	while i < nn do begin
	  PtLen[i] := 0;
	  inc(i);
	end;
	try
	  MakeTable(nn, @PtLen, 8, @PtTable);
	except
	  begin
		raise;
		Exit;
	  end;
	end;
  end;
end;

procedure ReadCLen;
var
  i, c, n: TwoByteInt;
  mask: Word;
begin
  n := GetBits(CBIT);
  if n = 0 then begin
	c := GetBits(CBIT);
	for i := 0 to Pred(NC) do begin
	  CLen^[i] := 0;
	end;
	for i := 0 to 4095 do begin
	  CTable^[i] := c;
	end;
  end else begin
	i := 0;
	while i < n do begin
	  c := PtTable[BitBuf shr (BITBUFSIZ - 8)];
	  if c >= NT then begin
		mask := 1 shl (BITBUFSIZ - 9);
		repeat
		  if (BitBuf and mask) <> 0 then begin
			c := Right^[c];
		  end else begin
			c := Left^[c];
		  end;
		  mask := mask shr 1;
		until c < NT;
	  end;
	  FillBuf(PtLen[c]);
	  if c <= 2 then begin
		if c = 1 then begin
		  c := 2 + GetBits(4);
		end else begin
		  if c = 2 then begin
			c := 19 + GetBits(CBIT);
		  end;
		end;
		while c >= 0 do begin
		  CLen^[i] := 0;
		  inc(i);
		  DEC(c);
		end;
	  end else begin
		CLen^[i] := c - 2;
		inc(i);
	  end;
	end;
	while i < NC do begin
	  CLen^[i] := 0;
	  inc(i);
	end;
	try
	  MakeTable(NC, PByte(CLen), 12, PWord(CTable));
	except
	  begin
		raise;
		Exit;
	  end;
	end;
  end;
end;

function DecodeC: Word;
var
  j, mask: Word;
begin
  if BlockSize = 0 then begin
	BlockSize := GetBits(16);
	ReadPtLen(NT, TBIT, 3);
	ReadCLen;
	ReadPtLen(NP, PBIT, -1);
  end;
  DEC(BlockSize);
  j := CTable^[BitBuf shr (BITBUFSIZ - 12)];
  if j >= NC then begin
	mask := 1 shl (BITBUFSIZ - 13);
	repeat
	  if (BitBuf and mask) <> 0 then begin
		j := Right^[j];
	  end else begin
		j := Left^[j];
	  end;
	  mask := mask shr 1;
	until j < NC;
  end;
  FillBuf(CLen^[j]);
  DecodeC := j;
end;

function DecodeP: Word;
var
  j, mask: Word;
begin
  j := PtTable[BitBuf shr (BITBUFSIZ - 8)];
  if j >= NP then begin
	mask := 1 shl (BITBUFSIZ - 9);
	repeat
	  if (BitBuf and mask) <> 0 then begin
		j := Right^[j];
	  end else begin
		j := Left^[j];
	  end;
	  mask := mask shr 1;
	until j < NP;
  end;
  FillBuf(PtLen[j]);
  if j <> 0 then begin
	DEC(j);
	j := (1 shl j) + GetBits(j);
  end;
  DecodeP := j;
end;

{ declared as static vars }
var
  decode_i: Word;
  decode_j: TwoByteInt;

procedure DecodeBuffer(count: Word; Buffer: PByte);
var
  c, r: Word;
begin
  r := 0;
  DEC(decode_j);
  while (decode_j >= 0) do begin
	Buffer^[r] := Buffer^[decode_i];
	decode_i := Succ(decode_i) and Pred(DICSIZ);
	inc(r);
	if r = count then begin
	  Exit;
	end;
	DEC(decode_j);
  end;
  while TRUE do begin
	c := DecodeC;
	if c <= UCHARMAX then begin
	  Buffer^[r] := c;
	  inc(r);
	  if r = count then begin
		Exit;
	  end;
	end else begin
	  decode_j := c - (UCHARMAX + 1 - THRESHOLD);
	  decode_i := (LongInt(r) - DecodeP - 1) and Pred(DICSIZ);
	  DEC(decode_j);
	  while decode_j >= 0 do begin
		Buffer^[r] := Buffer^[decode_i];
		decode_i := Succ(decode_i) and Pred(DICSIZ);
		inc(r);
		if r = count then begin
		  Exit;
		end;
		DEC(decode_j);
	  end;
	end;
  end;
end;

procedure Decode;
var
  p: PByte;
  l: LongInt;
  a: Word;
begin
  { Initialize decoder variables }
  GetMem(p, DICSIZ);
  InitGetBits;
  BlockSize := 0;
  decode_j := 0;
  { skip file size }
  l := OrigSize;
  DEC(CompSize, 4);
  { unpacks the file }
  while l > 0 do begin
	if l > DICSIZ then begin
	  a := DICSIZ;
	end else begin
	  a := l;
	end;
	DecodeBuffer(a, p);
	OutFile.Write(p^, a);
	DEC(l, a);
  end;
  FreeMem(p, DICSIZ);
end;

{ ********************************* Compression ******************************** }

{ -------------------------------- Huffman part -------------------------------- }

procedure CountLen(i: TwoByteInt);
begin
  if i < n then begin
	if Depth < 16 then begin
	  inc(LenCnt[Depth]);
	end else begin
	  inc(LenCnt[16]);
	end;
  end else begin
	inc(Depth);
	CountLen(Left^[i]);
	CountLen(Right^[i]);
	DEC(Depth);
  end;
end;

procedure MakeLen(root: TwoByteInt);
var
  i, k: TwoByteInt;
  cum: Word;
begin
  for i := 0 to 16 do begin
	LenCnt[i] := 0;
  end;
  CountLen(root);
  cum := 0;
  for i := 16 downto 1 do begin
	inc(cum, LenCnt[i] shl (16 - i));
  end;
  while cum <> 0 do begin
	DEC(LenCnt[16]);
	for i := 15 downto 1 do begin
	  if LenCnt[i] <> 0 then begin
		DEC(LenCnt[i]);
		inc(LenCnt[Succ(i)], 2);
		BREAK;
	  end;
	end;
	DEC(cum);
  end;
  for i := 16 downto 1 do begin
	k := Pred(LongInt(LenCnt[i]));
	while k >= 0 do begin
	  DEC(k);
	  Len^[SortPtr^[0]] := i;
	  asm
		ADD WORD PTR SortPtr,2; { SortPtr:=addr(SortPtr^[1]); }
	  end;
	end;
  end;
end;

procedure DownHeap(i: TwoByteInt);
var
  j, k: TwoByteInt;
begin
  k := Heap^[i];
  j := i shl 1;
  while (j <= HeapSize) do begin
	if (j < HeapSize) and (Freq^[Heap^[j]] > Freq^[Heap^[Succ(j)]]) then begin
	  inc(j);
	end;
	if Freq^[k] <= Freq^[Heap^[j]] then begin
	  BREAK;
	end;
	Heap^[i] := Heap^[j];
	i := j;
	j := i shl 1;
  end;
  Heap^[i] := k;
end;

procedure MakeCode(n: TwoByteInt; Len: PByte; Code: PWord);
var
  i, k: TwoByteInt;
  start: array [0 .. 17] of Word;
begin
  start[1] := 0;
  for i := 1 to 16 do begin
	start[Succ(i)] := (start[i] + LenCnt[i]) shl 1;
  end;
  for i := 0 to Pred(n) do begin
	k := Len^[i];
	Code^[i] := start[k];
	inc(start[k]);
  end;
end;

function MakeTree(NParm: TwoByteInt; Freqparm: PWord; LenParm: PByte; Codeparm: PWord): TwoByteInt;
var
  i, j, k, Avail: TwoByteInt;
begin
  n := NParm;
  Freq := Freqparm;
  Len := LenParm;
  Avail := n;
  HeapSize := 0;
  Heap^[1] := 0;
  for i := 0 to Pred(n) do begin
	Len^[i] := 0;
	if Freq^[i] <> 0 then begin
	  inc(HeapSize);
	  Heap^[HeapSize] := i;
	end;
  end;
  if HeapSize < 2 then begin
	Codeparm^[Heap^[1]] := 0;
	MakeTree := Heap^[1];
	Exit;
  end;
  for i := (HeapSize div 2) downto 1 do begin
	DownHeap(i);
  end;
  SortPtr := Codeparm;
  repeat
	i := Heap^[1];
	if i < n then begin
	  SortPtr^[0] := i;
	  asm
		ADD WORD PTR SortPtr,2; { SortPtr:=addr(SortPtr^[1]); }
	  end;
	end;
	Heap^[1] := Heap^[HeapSize];
	DEC(HeapSize);
	DownHeap(1);
	j := Heap^[1];
	if j < n then begin
	  SortPtr^[0] := j;
	  asm
		ADD WORD PTR SortPtr,2; { SortPtr:=addr(SortPtr^[1]); }
	  end;
	end;
	k := Avail;
	inc(Avail);
	Freq^[k] := Freq^[i] + Freq^[j];
	Heap^[1] := k;
	DownHeap(1);
	Left^[k] := i;
	Right^[k] := j;
  until HeapSize <= 1;
  SortPtr := Codeparm;
  MakeLen(k);
  MakeCode(NParm, LenParm, Codeparm);
  MakeTree := k;
end;

procedure CountTFreq;
var
  i, k, n, count: TwoByteInt;
begin
  for i := 0 to Pred(NT) do begin
	TFreq[i] := 0;
  end;
  n := NC;
  while (n > 0) and (CLen^[Pred(n)] = 0) do begin
	DEC(n);
  end;
  i := 0;
  while i < n do begin
	k := CLen^[i];
	inc(i);
	if k = 0 then begin
	  count := 1;
	  while (i < n) and (CLen^[i] = 0) do begin
		inc(i);
		inc(count);
	  end;
	  if count <= 2 then begin
		inc(TFreq[0], count);
	  end else begin
		if count <= 18 then begin
		  inc(TFreq[1]);
		end else begin
		  if count = 19 then begin
			inc(TFreq[0]);
			inc(TFreq[1]);
		  end else begin
			inc(TFreq[2]);
		  end;
		end;
	  end;
	end else begin
	  inc(TFreq[k + 2]);
	end;
  end;
end;

procedure WritePtLen(n, nBit, ispecial: TwoByteInt);
var
  i, k: TwoByteInt;
begin
  while (n > 0) and (PtLen[Pred(n)] = 0) do begin
	DEC(n);
  end;
  PutBits(nBit, n);
  i := 0;
  while (i < n) do begin
	k := PtLen[i];
	inc(i);
	if k <= 6 then begin
	  PutBits(3, k);
	end else begin
	  DEC(k, 3);
	  PutBits(k, (1 shl k) - 2);
	end;
	if i = ispecial then begin
	  while (i < 6) and (PtLen[i] = 0) do begin
		inc(i);
	  end;
	  PutBits(2, (i - 3) and 3);
	end;
  end;
end;

procedure WriteCLen;
var
  i, k, n, count: TwoByteInt;
begin
  n := NC;
  while (n > 0) and (CLen^[Pred(n)] = 0) do begin
	DEC(n);
  end;
  PutBits(CBIT, n);
  i := 0;
  while (i < n) do begin
	k := CLen^[i];
	inc(i);
	if k = 0 then begin
	  count := 1;
	  while (i < n) and (CLen^[i] = 0) do begin
		inc(i);
		inc(count);
	  end;
	  if count <= 2 then begin
		for k := 0 to Pred(count) do begin
		  PutBits(PtLen[0], PtCode[0]);
		end;
	  end else begin
		if count <= 18 then begin
		  PutBits(PtLen[1], PtCode[1]);
		  PutBits(4, count - 3);
		end else begin
		  if count = 19 then begin
			PutBits(PtLen[0], PtCode[0]);
			PutBits(PtLen[1], PtCode[1]);
			PutBits(4, 15);
		  end else begin
			PutBits(PtLen[2], PtCode[2]);
			PutBits(CBIT, count - 20);
		  end;
		end;
	  end;
	end else begin
	  PutBits(PtLen[k + 2], PtCode[k + 2]);
	end;
  end;
end;

procedure EncodeC(c: TwoByteInt);
begin
  PutBits(CLen^[c], CCode[c]);
end;

procedure EncodeP(p: Word);
var
  c, q: Word;
begin
  c := 0;
  q := p;
  while q <> 0 do begin
	q := q shr 1;
	inc(c);
  end;
  PutBits(PtLen[c], PtCode[c]);
  if c > 1 then begin
	PutBits(Pred(c), p and ($FFFF shr (17 - c)));
  end;
end;

procedure SendBlock;
var
  i, k, flags, root, Pos, size: Word;
begin
  root := MakeTree(NC, @CFreq, PByte(CLen), @CCode);
  size := CFreq[root];
  PutBits(16, size);
  if root >= NC then begin
	CountTFreq;
	root := MakeTree(NT, @TFreq, @PtLen, @PtCode);
	if root >= NT then begin
	  WritePtLen(NT, TBIT, 3);
	end else begin
	  PutBits(TBIT, 0);
	  PutBits(TBIT, root);
	end;
	WriteCLen;
  end else begin
	PutBits(TBIT, 0);
	PutBits(TBIT, 0);
	PutBits(CBIT, 0);
	PutBits(CBIT, root);
  end;
  root := MakeTree(NP, @PFreq, @PtLen, @PtCode);
  if root >= NP then begin
	WritePtLen(NP, PBIT, -1);
  end else begin
	PutBits(PBIT, 0);
	PutBits(PBIT, root);
  end;
  Pos := 0;
  for i := 0 to Pred(size) do begin
	if (i and 7) = 0 then begin
	  flags := Buf^[Pos];
	  inc(Pos);
	end else begin
	  flags := flags shl 1;
	end;
	if (flags and (1 shl 7)) <> 0 then begin
	  k := Buf^[Pos] + (1 shl 8);
	  inc(Pos);
	  EncodeC(k);
	  k := Buf^[Pos] shl 8;
	  inc(Pos);
	  inc(k, Buf^[Pos]);
	  inc(Pos);
	  EncodeP(k);
	end else begin
	  k := Buf^[Pos];
	  inc(Pos);
	  EncodeC(k);
	end;
  end;
  for i := 0 to Pred(NC) do begin
	CFreq[i] := 0;
  end;
  for i := 0 to Pred(NP) do begin
	PFreq[i] := 0;
  end;
end;

procedure Output(c, p: Word);
begin
  OutputMask := OutputMask shr 1;
  if OutputMask = 0 then begin
	OutputMask := 1 shl 7;
	if (OutputPos >= WINDOWSIZE - 24) then begin
	  SendBlock;
	  OutputPos := 0;
	end;
	CPos := OutputPos;
	inc(OutputPos);
	Buf^[CPos] := 0;
  end;
  Buf^[OutputPos] := c;
  inc(OutputPos);
  inc(CFreq[c]);
  if c >= (1 shl 8) then begin
	Buf^[CPos] := Buf^[CPos] or OutputMask;
	Buf^[OutputPos] := (p shr 8);
	inc(OutputPos);
	Buf^[OutputPos] := p;
	inc(OutputPos);
	c := 0;
	while p <> 0 do begin
	  p := p shr 1;
	  inc(c);
	end;
	inc(PFreq[c]);
  end;
end;

{ ------------------------------- Lempel-Ziv part ------------------------------ }

procedure InitSlide;
var
  i: Word;
begin
  for i := DICSIZ to (DICSIZ + UCHARMAX) do begin
	Level^[i] := 1;
	{$IFDEF PERCOLATE}
	Position^[i] := NUL;
	{$ENDIF}
  end;
  for i := DICSIZ to Pred(2 * DICSIZ) do begin
	Parent^[i] := NUL;
  end;
  Avail := 1;
  for i := 1 to DICSIZ - 2 do begin
	Next^[i] := Succ(i);
  end;
  Next^[Pred(DICSIZ)] := NUL;
  for i := (2 * DICSIZ) to MAXHASHVAL do begin
	Next^[i] := NUL;
  end;
end;

{ Hash function }
function Hash(p: TwoByteInt; c: Byte): TwoByteInt;
begin
  Hash := p + (c shl (DICBIT - 9)) + 2 * DICSIZ;
end;

function Child(q: TwoByteInt; c: Byte): TwoByteInt;
var
  r: TwoByteInt;
begin
  r := Next^[Hash(q, c)];
  Parent^[NUL] := q;
  while Parent^[r] <> q do begin
	r := Next^[r];
  end;
  Child := r;
end;

procedure MakeChild(q: TwoByteInt; c: Byte; r: TwoByteInt);
var
  h, t: TwoByteInt;
begin
  h := Hash(q, c);
  t := Next^[h];
  Next^[h] := r;
  Next^[r] := t;
  Prev^[t] := r;
  Prev^[r] := h;
  Parent^[r] := q;
  inc(ChildCount^[q]);
end;

procedure Split(old: TwoByteInt);
var
  new, t: TwoByteInt;
begin
  new := Avail;
  Avail := Next^[new];
  ChildCount^[new] := 0;
  t := Prev^[old];
  Prev^[new] := t;
  Next^[t] := new;
  t := Next^[old];
  Next^[new] := t;
  Prev^[t] := new;
  Parent^[new] := Parent^[old];
  Level^[new] := MatchLen;
  Position^[new] := Pos;
  MakeChild(new, Text^[MatchPos + MatchLen], old);
  MakeChild(new, Text^[Pos + MatchLen], Pos);
end;

procedure InsertNode;
var
  q, r, j, t: TwoByteInt;
  c: Byte;
  t1, t2: PChar;
begin
  if MatchLen >= 4 then begin
	DEC(MatchLen);
	r := Succ(MatchPos) or DICSIZ;
	q := Parent^[r];
	while q = NUL do begin
	  r := Next^[r];
	  q := Parent^[r];
	end;
	while Level^[q] >= MatchLen do begin
	  r := q;
	  q := Parent^[q];
	end;
	t := q;
	{$IFDEF PERCOLATE}
	while Position^[t] < 0 do begin
	  Position^[t] := Pos;
	  t := Parent^[t];
	end;
	if t < DICSIZ then begin
	  Position^[t] := Pos or PERCFLAG;
	end;
	{$ELSE}
	while t < DICSIZ do begin
	  Position^[t] := Pos;
	  t := Parent^[t];
	end;
	{$ENDIF}
  end else begin
	q := Text^[Pos] + DICSIZ;
	c := Text^[Succ(Pos)];
	r := Child(q, c);
	if r = NUL then begin
	  MakeChild(q, c, Pos);
	  MatchLen := 1;
	  Exit;
	end;
	MatchLen := 2;
  end;
  while TRUE do begin
	if r >= DICSIZ then begin
	  j := MAXMATCH;
	  MatchPos := r;
	end else begin
	  j := Level^[r];
	  MatchPos := Position^[r] and not PERCFLAG;
	end;
	if MatchPos >= Pos then begin
	  DEC(MatchPos, DICSIZ);
	end;
	t1 := Addr(Text^[Pos + MatchLen]);
	t2 := Addr(Text^[MatchPos + MatchLen]);
	while MatchLen < j do begin
	  if t1^ <> t2^ then begin
		Split(r);
		Exit;
	  end;
	  inc(MatchLen);
	  inc(t1);
	  inc(t2);
	end;
	if MatchLen >= MAXMATCH then begin
	  BREAK;
	end;
	Position^[r] := Pos;
	q := r;
	r := Child(q, ORD(t1^));
	if r = NUL then begin
	  MakeChild(q, ORD(t1^), Pos);
	  Exit;
	end;
	inc(MatchLen);
  end;
  t := Prev^[r];
  Prev^[Pos] := t;
  Next^[t] := Pos;
  t := Next^[r];
  Next^[Pos] := t;
  Prev^[t] := Pos;
  Parent^[Pos] := q;
  Parent^[r] := NUL;
  Next^[r] := Pos;
end;

procedure DeleteNode;
var
  r, s, t, u: TwoByteInt;
  {$IFDEF PERCOLATE}
  q: TwoByteInt;
  {$ENDIF}
begin
  if Parent^[Pos] = NUL then begin
	Exit;
  end;
  r := Prev^[Pos];
  s := Next^[Pos];
  Next^[r] := s;
  Prev^[s] := r;
  r := Parent^[Pos];
  Parent^[Pos] := NUL;
  DEC(ChildCount^[r]);
  if (r >= DICSIZ) or (ChildCount^[r] > 1) then begin
	Exit;
  end;
  {$IFDEF PERCOLATE}
  t := Position^[r] and not PERCFLAG;
  {$ELSE}
  t := Position^[r];
  {$ENDIF}
  if t >= Pos then begin
	DEC(t, DICSIZ);
  end;
  {$IFDEF PERCOLATE}
  s := t;
  q := Parent^[r];
  u := Position^[q];
  while (u and PERCFLAG) <> 0 do begin
	u := u and not PERCFLAG;
	if u >= Pos then begin
	  DEC(u, DICSIZ);
	end;
	if u > s then begin
	  s := u;
	end;
	Position^[q] := s or DICSIZ;
	q := Parent^[q];
	u := Position^[q];
  end;
  if q < DICSIZ then begin
	if u >= Pos then begin
	  DEC(u, DICSIZ);
	end;
	if u > s then begin
	  s := u;
	end;
	Position^[q] := s or DICSIZ or PERCFLAG;
  end;
  {$ENDIF}
  s := Child(r, Text^[t + Level^[r]]);
  t := Prev^[s];
  u := Next^[s];
  Next^[t] := u;
  Prev^[u] := t;
  t := Prev^[r];
  Next^[t] := s;
  Prev^[s] := t;
  t := Next^[r];
  Prev^[t] := s;
  Next^[s] := t;
  Parent^[s] := Parent^[r];
  Parent^[r] := NUL;
  Next^[r] := Avail;
  Avail := r;
end;

procedure GetNextMatch;
var
  n: TwoByteInt;
begin
  DEC(Remainder);
  inc(Pos);
  if Pos = 2 * DICSIZ then begin
	move(Text^[DICSIZ], Text^[0], DICSIZ + MAXMATCH);
	n := InFile.read(Text^[DICSIZ + MAXMATCH], DICSIZ);
	inc(Remainder, n);
	Pos := DICSIZ;
  end;
  DeleteNode;
  InsertNode;
end;

procedure Encode;
var
  LastMatchLen, LastMatchPos: TwoByteInt;
begin
  { initialize encoder variables }
  GetMem(Text, 2 * DICSIZ + MAXMATCH);
  GetMem(Level, DICSIZ + UCHARMAX + 1);
  GetMem(ChildCount, DICSIZ + UCHARMAX + 1);
  {$IFDEF PERCOLATE}
  GetMem(Position, (DICSIZ + UCHARMAX + 1) * sizeof(Word));
  {$ELSE}
  GetMem(Position, (DICSIZ) * sizeof(Word));
  {$ENDIF}
  GetMem(Parent, (DICSIZ * 2) * sizeof(Word));
  GetMem(Prev, (DICSIZ * 2) * sizeof(Word));
  GetMem(Next, (MAXHASHVAL + 1) * sizeof(Word));

  Depth := 0;
  InitSlide;
  GetMem(Buf, WINDOWSIZE);
  Buf^[0] := 0;
  FillChar(CFreq, sizeof(CFreq), 0);
  FillChar(PFreq, sizeof(PFreq), 0);
  OutputPos := 0;
  OutputMask := 0;
  InitPutBits;
  Remainder := InFile.read(Text^[DICSIZ], DICSIZ + MAXMATCH);
  MatchLen := 0;
  Pos := DICSIZ;
  InsertNode;
  if MatchLen > Remainder then begin
	MatchLen := Remainder;
  end;
  while Remainder > 0 do begin
	LastMatchLen := MatchLen;
	LastMatchPos := MatchPos;
	GetNextMatch;
	if MatchLen > Remainder then begin
	  MatchLen := Remainder;
	end;
	if (MatchLen > LastMatchLen) or (LastMatchLen < THRESHOLD) then begin
	  Output(Text^[Pred(Pos)], 0);
	end else begin
	  Output(LastMatchLen + (UCHARMAX + 1 - THRESHOLD), (Pos - LastMatchPos - 2) and Pred(DICSIZ));
	  DEC(LastMatchLen);
	  while LastMatchLen > 0 do begin
		GetNextMatch;
		DEC(LastMatchLen);
	  end;
	  if MatchLen > Remainder then begin
		MatchLen := Remainder;
	  end;
	end;
  end;
  { flush buffers }
  SendBlock;
  PutBits(7, 0);
  if BufPtr <> 0 then begin
	OutFile.Write(Buffer^, BufPtr);
  end;

  FreeMem(Buf, WINDOWSIZE);
  FreeMem(Next, (MAXHASHVAL + 1) * sizeof(Word));
  FreeMem(Prev, (DICSIZ * 2) * sizeof(Word));
  FreeMem(Parent, (DICSIZ * 2) * sizeof(Word));
  {$IFDEF PERCOLATE}
  FreeMem(Position, (DICSIZ + UCHARMAX + 1) * sizeof(Word));
  {$ELSE}
  FreeMem(Position, (DICSIZ) * sizeof(Word));
  {$ENDIF}
  FreeMem(ChildCount, DICSIZ + UCHARMAX + 1);
  FreeMem(Level, DICSIZ + UCHARMAX + 1);
  FreeMem(Text, 2 * DICSIZ + MAXMATCH);
end;

{ ****************************** LH5 as Unit Procedures ************************ }
procedure FreeMemory;
begin
  if CLen <> nil then begin
	Dispose(CLen);
  end;
  CLen := nil;
  if CTable <> nil then begin
	Dispose(CTable);
  end;
  CTable := nil;
  if Right <> nil then begin
	Dispose(Right);
  end;
  Right := nil;
  if Left <> nil then begin
	Dispose(Left);
  end;
  Left := nil;
  if Buffer <> nil then begin
	Dispose(Buffer);
  end;
  Buffer := nil;
  if Heap <> nil then begin
	Dispose(Heap);
  end;
  Heap := nil;
end;

procedure InitMemory;
begin
  { In should be harmless to call FreeMemory here, since it won't free
	unallocated memory (i.e., nil pointers).
	So let's call it in case an exception was thrown at some point and
	memory wasn't entirely freed. }
  FreeMemory;
  new(Buffer);
  new(Left);
  new(Right);
  new(CTable);
  new(CLen);
  FillChar(Buffer^, sizeof(Buffer^), 0);
  FillChar(Left^, sizeof(Left^), 0);
  FillChar(Right^, sizeof(Right^), 0);
  FillChar(CTable^, sizeof(CTable^), 0);
  FillChar(CLen^, sizeof(CLen^), 0);

  decode_i := 0;
  BitBuf := 0;
  n := 0;
  HeapSize := 0;
  SubBitBuf := 0;
  BitCount := 0;
  BufPtr := 0;
  FillChar(PtTable, sizeof(PtTable), 0);
  FillChar(PtLen, sizeof(PtLen), 0);
  BlockSize := 0;

  { The following variables are used by the compression engine only }
  new(Heap);
  FillChar(Heap^, sizeof(Heap^), 0);
  FillChar(LenCnt, sizeof(LenCnt), 0);
  Depth := 0;
  FillChar(CFreq, sizeof(CFreq), 0);
  FillChar(PFreq, sizeof(PFreq), 0);
  FillChar(TFreq, sizeof(TFreq), 0);
  FillChar(CCode, sizeof(CCode), 0);
  FillChar(PtCode, sizeof(PtCode), 0);
  CPos := 0;
  OutputPos := 0;
  OutputMask := 0;
  Pos := 0;
  MatchPos := 0;
  Avail := 0;
  Remainder := 0;
  MatchLen := 0;
end;

{ ******************************** Interface Procedures ************************ }
procedure LHACompress(InStr, OutStr: TStream);
begin
  InitMemory;
  try
	InFile := InStr;
	OutFile := OutStr;
	OrigSize := InFile.size - InFile.Position;
	CompSize := 0;
	OutFile.Write(OrigSize, 4);
	Encode;
  finally
	FreeMemory;
  end;
end;

procedure LHAExpand(InStr, OutStr: TStream);
begin
  try
	InitMemory;
	InFile := InStr;
	OutFile := OutStr;
	CompSize := InFile.size - InFile.Position;
	InFile.read(OrigSize, 4);
	Decode;
  finally
	FreeMemory;
  end;
end;

initialization

CLen := nil;
CTable := nil;
Right := nil;
Left := nil;
Buffer := nil;
Heap := nil;
Blowfish_Done;

finalization

Blowfish_Done;

end.
