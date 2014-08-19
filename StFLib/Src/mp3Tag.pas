{$IFDEF mp3Tag}
    {$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I StFLib.inc}

{
----------------------------------------------------------
MAS-CompMaker was used to generate this code
MAS-CompMaker, 2000-2001® Mats Asplund
----------------------------------------------------------

Component Name: Tmp3Tag
        Author: Slightly improved and redone from a class made by
                Andrey V. Sorokin, into a component by Mats Asplund.
      Creation: 2001-11-08
       Version: 1.0
   Description: A component for reading and writing mp3-tags.
        Credit: Andrey V.Sorokin
                Saint-Petersburg, Russia
                anso@mail.ru
                anso@usa.net
                http://anso.da.ru
                http://anso.virtualave.net
        E-mail: masprod@telia.com
          Site: http://go.to/masdp
  Legal issues: Copyright(c) 1999,2000 by Andrey V.Sorokin.
                All rights reserved 2001® by Mats Asplund.

Usage:
  This software is provided 'as-is', without any express or
  implied warranty.  In no event will the author be held liable
  for any  damages arising from the use of this software.

  Permission is granted to anyone to use this software for any
  purpose, including commercial applications, and to alter it
  and redistribute it freely, subject to the following
  restrictions:

  1. The origin of this software must not be misrepresented,
     you must not claim that you wrote the original software.
     If you use this software in a product, an acknowledgment
     in the product documentation would be appreciated but is
     not required.

  2. Altered source versions must be plainly marked as such, and
     must not be misrepresented as being the original software.

  3. This notice may not be removed or altered from any source
     distribution.

  4. If you decide to use this software in any of your applications.
     Send me an EMail and tell me about it.

Quick Reference:

  Tmp3Tag inherits from TComponent.

  Key-Properties:

    TagPresent:        True if loaded tag seems to be OK.

    TagModified:       True if anything changed.

    v1Tag:             True if ID3v1, false if ID3v1.1

    Title:             Music title

    Artist:            Performer

    Year:              Year as 4-chars string.

    Album:             Album title

    Track:             Track number (only if not v1)

    Comment:           Comment

    GenreID:           Genre ID (see fID3Genres which contains genres list as
                       TStrings)

    Genre:             Genre as string


  Key-Methods:

    LoadTag:           Load tag from ABuf (max ABufSz)

     LoadTagFromStream: Find ID3tag in AStream (max AStreamSz bytes from current
                        position) and Load it. If success then returns offset to
                        tag, else -1.

     LoadTagFromFile:   Load ID3Tag from file AFileName

     SaveTag:           Write tag into ABuf (max ABufSz bytes), clear Modified.

     SaveTagToStream:   Write tag ito current position of AStream, clear Modified.

     SaveTagToFile:     Add/Update tag in file AFileName, clear Modified.

     DeleteTagFromFile: Delete tag from file AFileName (if it exists).
----------------------------------------------------------------------------------------------------------------------------------}

unit mp3Tag;

interface

uses
    SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls;

type

    Tmp3Tag = Class (TComponent)
    private
        fAbout : string;
        fModified : Boolean;
        fOk : Boolean;
        fv1 : Boolean;
        fTrack : Integer;
        fGenreID : Integer;
        fArtist : string;
        FTitle : string;
        fAlbum : string;
        fYear : string;
        fComment : string;
        fGenre : string;
        fID3Genres : TStringList;
        procedure SetCop(Value : string);
        procedure SetGenreID(AGenreID : Integer);
        procedure SetTrack(ATrack : Integer);
        procedure Setv1(b : Boolean);
        procedure SetField(AIdx : Integer; S : string);
        procedure Reset;
        procedure SetGenre(const Value : string);
        procedure SetID3Genres(const Value : TStringList);
    public
        constructor Create(AOwner : TComponent); override;
        destructor Destroy; override;
        procedure LoadTag(const ABuf; ABufSz : Integer);
        procedure LoadTagFromStream(AStream : TStream; AStreamSz : Integer);
        procedure LoadTagFromFile(const aFileName : string);
        procedure SaveTag(var ABuf; ABufSz : Integer);
        procedure SaveTagToStream(AStream : TStream);
        procedure SaveTagToFile(const aFileName : string);
        procedure DeleteTagFromFile(const aFileName : string);
    published
        property TagPresent : Boolean read fOk write fOk;
        property TagModified : Boolean read fModified;
        property v1Tag : Boolean read fv1 write Setv1;
        property Title : string index 1 read FTitle write SetField;
        property Artist : string index 2 read fArtist write SetField;
        property Year : string index 3 read fYear write SetField;
        property Album : string index 4 read fAlbum write SetField;
        property Track : Integer read fTrack write SetTrack; // ID3v1.1 only
        property Comment : string index 5 read fComment write SetField;
        property GenreID : Integer read fGenreID write SetGenreID;
        property Genre : string read fGenre write SetGenre;
        property Genres : TStringList read fID3Genres write SetID3Genres;
        property About : string read fAbout write SetCop;
    end;

procedure Register;

implementation

const
    ID3v1TagLen = 128;

procedure Register;
begin
    RegisterComponents('Super', [Tmp3Tag]);
end;

constructor Tmp3Tag.Create(AOwner : TComponent);
    //----------------------------------------------------------------------------------------------------------------------------------
begin
    inherited Create(AOwner);
    fID3Genres := TStringList.Create;
    fID3Genres.Duplicates := dupAccept;
    fID3Genres.Sorted := FALSE;
    fID3Genres.CommaText :=
        '"Blues","Classic Rock","Country","Dance","Disco","Funk","Grunge",'
        + '"Hip-Hop","Jazz","Metal","New Age","Oldies","Other","Pop",'
        + '"R&B","Rap","Reggae","Rock","Techno","Industrial","Alternative",'
        + '"Ska","Death Metal","Pranks","Soundtrack","Euro-Techno","Ambient",'
        + '"Trip-Hop","Vocal","Jazz+Funk","Fusion","Trance","Classical",'
        + '"Instrumental","Acid","House","Game","Sound Clip","Gospel",'
        + '"Noise","AlternRock","Bass","Soul","Punk","Space","Meditative",'
        + '"Instrumental Pop","Instrumental Rock","Ethnic","Gothic",'
        + '"Darkwave","Techno-Industrial","Electronic","Pop-Folk","Eurodance",'
        + '"Dream","Southern Rock","Comedy","Cult","Gangsta","Top 40",'
        + '"Christian Rap","Pop/Funk","Jungle","Native American","Cabaret",'
        + '"New Wave","Psychedelic","Rave","Showtunes","Trailer","Lo-Fi",'
        + '"Tribal","Acid Punk","Acid Jazz","Polka","Retro","Musical",'
        + '"Rock & Roll","Hard Rock",' // 79 standard genres
        + '"Folk","Folk/Rock","National Folk","Swing","Fast Fusion",'
        + '"Bebob","Latin","Revival","Celtic","Bluegrass","Avantgarde",'
        + '"Gothic Rock","Progressive Rock","Psychedelic Rock",'
        + '"Symphonic Rock","Slow Rock","Big Band","Chorus","Easy Listening",'
        + '"Acoustic","Humour","Speech","Chanson","Opera","Chamber Music",'
        + '"Sonata","Symphony","Booty Bass","Primus","Porn Groove",'
        + '"Satire","Slow Jam","Club","Tango","Samba","Folklore",'
        + '"Ballad","Power Ballad","Rhythmic Soul","Freestyle",'
        + '"Duet","Punk Rock","Drum Solo","Acapella","Euro-House",'
        + '"Dance Hall", "Goa", "Drum & Bass", "Club-House", "Hardcore",'
        + '"Terror", "Indie", "BritPop", "Negerpunk", "Polsk Punk", "Beat",'
        + '"Christian Gangs", "Heavy Metal", "Black Metal", "Crossover",'
        + '"Contemporary Ch?", "Cristian Rock", "Merengue", "Salsa",'
        + '"Thrash Metal", "Anime", "JPop", "Synthpop", ""'; // 0 .. 93h

    fGenreId := 0;
    fGenre := 'Blues';
    fAbout := 'Version 1.0, 2001® Andrey V.Sorokin & Mats Asplund';
end;

procedure Tmp3Tag.DeleteTagFromFile(const aFileName : string);
//----------------------------------------------------------------------------------------------------------------------------------
var
    F : TStream;
    S : string;
begin
    F := TFileStream.Create(aFileName, fmOpenReadWrite or fmShareExclusive);
    try
        if F.Size >= ID3v1TagLen then begin
            F.Position := (F.Size - ID3v1TagLen);
            SetString(S, NIL, 3);
            F.ReadBuffer(S[1], 3);
            if S = 'TAG' then begin
                F.Size := (F.Size - ID3v1TagLen);
            end;
        end;
    finally
        F.Free;
    end;
end;

destructor Tmp3Tag.Destroy;
    //----------------------------------------------------------------------------------------------------------------------
begin
    Self.fID3Genres.Free;
    inherited;
end;

procedure Tmp3Tag.LoadTag(const ABuf; ABufSz : Integer);
//----------------------------------------------------------------------------------------------------------------------------------
var
    b : array[0..ID3v1TagLen - 1] of Byte absolute ABuf;
    GenreIdx, i : Integer;
    //..................................................................................................................................
    function LSRBufStr(APos, ALen : Integer) : string;
    begin
        SetString(Result, NIL, ALen);
        Move(b[APos], Result[1], ALen);
        if Pos(#0, Result) > 0 then        begin
            Result := Copy(Result, 1, Pos(#0, Result) - 1);
        end;
        Result := TrimRight(Result);
    end;
    //..................................................................................................................................
begin
    // save genres IDs for genres list resorting ability
    Reset;
    for i := 0 to (fID3Genres.Count - 1) do begin
        fID3Genres.Objects[i] := TObject(i);
    end;

    fOk := FALSE;
    fModified := FALSE;
    if (ABufSz < ID3v1TagLen) then begin
        Exit;
    end;
    if LSRBufStr(0, 3) = 'TAG' then begin
        fOk := TRUE;
        FTitle := LSRBufStr(3, 30);
        fArtist := LSRBufStr(33, 30);
        fAlbum := LSRBufStr(63, 30);
        fYear := LSRBufStr(93, 4);
        fComment := LSRBufStr(97, 30);
        if (b[97 + 28] = 0) and (b[97 + 29] <> 0) then begin
            fv1 := FALSE;
            fTrack := b[97 + 29];
        end else begin
            fv1 := TRUE;
            fTrack := 0;
        end;
        fGenreID := b[127];
        GenreIdx := fID3Genres.IndexOfObject(TObject(fGenreID));
        if GenreIdx >= 0 then begin
            fGenre := fID3Genres[GenreIdx];
        end else begin
            fGenre := EmptyStr;
        end;
    end;
end;

procedure Tmp3Tag.LoadTagFromFile(const aFileName : string);
//----------------------------------------------------------------------------------------------------------------------------------
var
    F : TStream;
begin
    F := TFileStream.Create(aFileName, fmOpenRead or fmShareDenyNone);
    try
        LoadTagFromStream(F, F.Size);
    finally
        F.Free;
    end;
end;

procedure Tmp3Tag.LoadTagFromStream(AStream : TStream; AStreamSz : Integer);
//----------------------------------------------------------------------------------------------------------------------------------
var
    Buf : array[0..ID3v1TagLen - 1] of Byte;
begin
    Reset;
    if (AStreamSz >= ID3v1TagLen) then begin
        AStream.Position := AStream.Position + AStreamSz - ID3v1TagLen;
        AStream.ReadBuffer(Buf, ID3v1TagLen);
        LoadTag(Buf, ID3v1TagLen);
    end else begin
        fOk := FALSE;
    end;
end;

procedure Tmp3Tag.Reset;
//----------------------------------------------------------------------------------------------------------------------------------
begin
    fGenreID := 94; {empty string}
    fArtist := EmptyStr;
    FTitle := EmptyStr;
    fAlbum := EmptyStr;
    fYear  := EmptyStr;
    fComment := EmptyStr;
    fGenre := EmptyStr;
end;

procedure Tmp3Tag.SaveTag(var ABuf; ABufSz : Integer);
//----------------------------------------------------------------------------------------------------------------------------------
var
    b : array[0..ID3v1TagLen - 1] of Byte absolute ABuf;
    //..................................................................................................................................
    procedure LSRBufStr(APos, ALen : Integer; const AStr : string);
    var
        Len : Integer;
    begin
        Len := Length(AStr);
        if Len > ALen then begin
            Len := ALen;
        end;
        Move(AStr[1], b[APos], Len);
        if Len < ALen then begin
            FillChar(b[APos + Len], ALen - Len, $20);
        end;
    end;
    //..................................................................................................................................
begin
    if ABufSz < ID3v1TagLen then begin
        raise Exception.Create('TID3v1Tag.Save: Buffer too small');
    end;
    // fOk ?!!
    LSRBufStr(0, 3, 'TAG');
    LSRBufStr(3, 30, FTitle);
    LSRBufStr(33, 30, fArtist);
    LSRBufStr(63, 30, fAlbum);
    LSRBufStr(93, 4, fYear);
    LSRBufStr(97, 30, fComment);
    if not fv1 then begin
        b[97 + 28] := 0;
        b[97 + 29] := fTrack;
    end;
    b[127] := fGenreID;
    fModified := FALSE;
end;

procedure Tmp3Tag.SaveTagToFile(const aFileName : string);
//----------------------------------------------------------------------------------------------------------------------------------
var
    F : TStream;
    S : string;
begin
    F := TFileStream.Create(aFileName, fmOpenReadWrite or fmShareDenyNone);
    try
        if F.Size >= ID3v1TagLen then begin
            F.Position := F.Size - ID3v1TagLen;
            SetString(S, NIL, 3);
            F.ReadBuffer(S[1], 3);
        end else begin
            S := 'NON';
        end;
        if S = 'TAG' then begin
            F.Position := (F.Size - ID3v1TagLen);
        end else begin
            F.Position := F.Size;
        end;
        SaveTagToStream(F);
    finally
        F.Free;
    end;
end;

procedure Tmp3Tag.SaveTagToStream(AStream : TStream);
//----------------------------------------------------------------------------------------------------------------------------------
var
    Buf : array[0..ID3v1TagLen - 1] of Byte;
begin
    SaveTag(Buf, ID3v1TagLen);
    AStream.WriteBuffer(Buf, ID3v1TagLen);
end;

procedure Tmp3Tag.SetCop(Value : string);
//----------------------------------------------------------------------------------------------------------------------------------
begin
    { TODO -oRoger -cLIB : Identificar a razao disso }
    Exit;
end;

procedure Tmp3Tag.SetField(AIdx : Integer; S : string);
//----------------------------------------------------------------------------------------------------------------------------------
    procedure LSRSetIt(const ANewValue : string; AMaxLen : Integer; var AField : string);
    begin
        if Copy(ANewValue, 1, AMaxLen) <> AField then begin
            AField := Copy(ANewValue, 1, AMaxLen);
            fModified := TRUE;
        end;
    end;
    //..................................................................................................................................
begin
    case AIdx of
        1    : begin
            LSRSetIt(S, 30, FTitle);
        end;
        2    : begin
            LSRSetIt(S, 30, fArtist);
        end;
        3    : begin
            LSRSetIt(S, 4, fYear);
        end;
        4    : begin
            LSRSetIt(S, 30, fAlbum);
        end;
        5    : begin
            if fv1 then begin
                LSRSetIt(S, 30, fComment);
            end else begin
                LSRSetIt(S, 28, fComment);
            end;
        end;
    end;
end;

procedure Tmp3Tag.SetGenre(const Value : string);
//----------------------------------------------------------------------------------------------------------------------------------
begin
    if Value <> fGenre then begin
        fGenre := Value;
        fGenreID := Genres.IndexOf(fGenre);
        fModified := TRUE;
    end;
end;

procedure Tmp3Tag.SetGenreID(AGenreID : Integer);
//----------------------------------------------------------------------------------------------------------------------------------
begin
    if fGenreID <> AGenreID then begin
        fGenreID := AGenreID;
        fGenre := Genres[fGenreID];
        fModified := TRUE;
    end;
end;

procedure Tmp3Tag.SetID3Genres(const Value : TStringList);
//----------------------------------------------------------------------------------------------------------------------------------
begin
    Exit; // List should be read-only
end;

procedure Tmp3Tag.SetTrack(ATrack : Integer);
//----------------------------------------------------------------------------------------------------------------------------------
begin
    if fTrack <> ATrack then begin
        fTrack := ATrack;
        fv1 := fTrack = 0;
        fModified := TRUE;
    end;
end;

procedure Tmp3Tag.Setv1(b : Boolean);
//----------------------------------------------------------------------------------------------------------------------------------
begin
    if fv1 <> b then begin
        fv1 := b;
        fModified := TRUE;
    end;
end;

end.


