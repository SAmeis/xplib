{$IFDEF IHTMLDB}
{$A+,B-,C-,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O-,P+,Q-,R+,S-,T-,U-,V+,W-,X+,Y+,Z4}
{$DEBUGINFO ON}
{$ELSE}
{$A+,B-,C-,D-,E-,F-,G+,H+,I+,J+,K-,L-,M-,N+,O+,P+,Q+,R+,S-,T-,U-,V+,W-,X+,Y-,Z4}
{$F+}
{$ENDIF}

unit IHTMLDB;

{
  HTML producer for datasets.
  Generates an HTML table from a table or query,
  displaying all the visible fields.

  Written by Keith Wood.
  Version 1.0 - July 22, 1998.
}

interface

uses
  Classes, DB, Graphics, IHTML4;

resourcestring
  { Error messages }
  sNoDataSet            = 'Nenhuma origem de dados( DataSet ) atribuída';
  sMissingLinkedField   = 'Nome de campo agragado falho'; //Missing linked field name
  sInvisibleLinkedField = 'Campo agregado não é visível'; //'Linked field is not visible';
  sMissingLinkTarget    = 'Falha buscando nome do campo para compor a URL';  //'Missing link target field name';
  sNoVisibleFields      = 'Nenhum campo desta tabela é visível';  //'No fields in this dataset are visible';
  sNoRecordsFound       = 'Nenhum campo presente na tabela em questão'; //There are no records in this dataset';


type
  { Change properties for a row }
  THTMLRowEvent = procedure(Sender: TObject; Row: THTMLTableRow) of object;
  { Change properties for a cell }
  THTMLCellEvent = function (Sender: TObject; Field: TField; Cell: THTMLTableDetail) : boolean of object; //Posto como funcao para indicar se a carga foi pre-feita

  { Generate an HTML table for database data }
  THTMLDataTable = class(THTMLTable)
  private
    FDataSet: TDataSet;
    FHeaders: Boolean;
    FHeaderBackground, FHeaderBorderColour, FHeaderBorderHighlight, FHeaderBorderShadow: TColor;
    FUseFieldAlign: Boolean;
    FLinkField: TField;
    FLinkTarget: TField;
    FOnHeaderRowProperties : THTMLRowEvent;
    FOnDetailRowProperties : THTMLRowEvent;
    FOnCellProperties: THTMLCellEvent;
  protected
    FDataLoaded: boolean;
  public
    constructor Create(dtsDataSet: TDataSet); reintroduce; virtual;
    function  AsHTML: String; override; stdcall;
    procedure Clear; override;
    function  LoadFromDataSet : integer; virtual;
    property DataLoaded : boolean read FDataLoaded;
    { The dataset from which we generate the HTML table }
    property DataSet: TDataSet read FDataSet write FDataSet;
    { Are headers to be generated ? }
    property Headers: Boolean read FHeaders write FHeaders default True;
    { The background colour for the header cells }
	 property HeaderBackground: TColor read FHeaderBackground write FHeaderBackground default clNone;
	 { The border colour for the header cells }
	 property HeaderBorderColour: TColor read FHeaderBorderColour write FHeaderBorderColour default clNone;
	 { The border highlight colour for the header cells }
	 property HeaderBorderHighlight: TColor read FHeaderBorderHighlight write FHeaderBorderHighlight default clNone;
	 { The border shadow colour for the header cells }
	 property HeaderBorderShadow: TColor read FHeaderBorderShadow write FHeaderBorderShadow default clNone;
	 { Are inidividual field alignments to be used in the table ? }
	 property UseFieldAlign: Boolean read FUseFieldAlign write FUseFieldAlign default True;
	 { The field that is to appear as a hotspot }
	 property LinkField: TField read FLinkField write FLinkField;
	 { The field that holds the destination for the link }
	 property LinkTarget: TField read FLinkTarget write FLinkTarget;

	 { Called before the header row is generated - change properties }
	 property OnHeaderRowProperties: THTMLRowEvent read FOnHeaderRowProperties write FOnHeaderRowProperties;
	 { Called before each detail row is generated - change properties }
	 property OnDetailRowProperties: THTMLRowEvent read FOnDetailRowProperties write FOnDetailRowProperties;
	 { Called before each cell is generated - change properties }
	 property OnCellProperties: THTMLCellEvent read FOnCellProperties write FOnCellProperties;
  end;

implementation


constructor THTMLDataTable.Create(dtsDataSet: TDataSet);
//----------------------------------------------------------------------------------------------------------------------
//Attach to specified dataset and initialise HTML table
begin
   inherited Create(AsPercentage(100), 2);  //Width 100%, BorderWidth 2
   FDataSet := dtsDataSet;
   FHeaders := True;
   FHeaderBackground := clNone;
   FHeaderBorderColour := clNone;
   FHeaderBorderHighlight := clNone;
   FHeaderBorderShadow := clNone;
   FUseFieldAlign := True;
end;


function THTMLDataTable.AsHTML : string;
//Generate the dataset contents as an HTML table
//----------------------------------------------------------------------------------------------------------------------
begin
   //>>HTML comes from inherited HTML table abilities
   if not Self.FDataLoaded then begin
       Self.LoadFromDataSet;
   end;
   Result := inherited AsHTML;
end;

function THTMLDataTable.LoadFromDataSet : integer;
//----------------------------------------------------------------------------------------------------------------------
const
   thAlignments: array [TAlignment] of THTMLTableAlignHoriz = (thLeft, thRight, thCentre);
var
   i: Integer;
   bVisible: Boolean;
   slsMemo: TStringList;
   bmkSave: TBookmark;
   htr: THTMLTableRow;
   hth: THTMLTableHeading;
   htd: THTMLTableDetail;
begin
   Result:=0;
   //Check that DataSet is set }
   if DataSet = nil then begin
       raise EHTMLError.Create(sNoDataSet);
   end;

   //Check linked fields
   if (LinkField <> nil) or (LinkTarget <> nil) then begin
       if LinkField = nil then begin
           raise EHTMLError.Create(sMissingLinkedField);
       end;
       if not LinkField.Visible then begin
           raise EHTMLError.Create(sInvisibleLinkedField);
       end;
       if LinkTarget = nil then begin
           raise EHTMLError.Create(sMissingLinkTarget);
       end;
   end;

   with DataSet do begin
       //Determine whether table has visible fields and records
       bVisible := False;
       for i := 0 to FieldCount - 1 do
           if Fields[i].Visible then begin
               bVisible := True;
               Break;
           end;
       if not bVisible then begin
           raise EHTMLError.Create(sNoVisibleFields);
       end;
       if RecordCount = 0 then begin
           raise EHTMLError.Create(sNoRecordsFound);
       end;

       //Remove any existing table structure
       Clear;

       //Write headers
       if Headers then begin
           htr := THTMLTableRow.Create;
           htr.AlignHoriz := AlignHoriz;
           htr.AlignVert := AlignVert;
           htr.Background := HeaderBackground;
           htr.BorderColour := HeaderBorderColour;
           htr.BorderHighlight := HeaderBorderHighlight;
           htr.BorderShadow := HeaderBorderShadow;
           //Allow user to set properties
           if Assigned(FOnHeaderRowProperties) then begin
               FOnHeaderRowProperties(Self, htr);
           end;
           //Add headings for each visible field
           for i := 0 to FieldCount - 1 do begin
               if Fields[i].Visible then begin
                   hth := THTMLTableHeading.Create(EscapeText(Fields[i].DisplayName));
                   if UseFieldAlign then begin
                       hth.AlignHoriz := thAlignments[Fields[i].Alignment];
                   end;
                   htr.Add(hth);
               end;
           end;
           Add(htr);
       end;

       //Don't update screen while processing
       DisableControls;
       bmkSave := GetBookmark; //Save data set position

       //Create temporary area for memo fields
       slsMemo := TStringList.Create;

       //Write contents of rows
       try
           First;
           while not EOF do begin   //Process all rows
               Inc( Result );
               //And create the row
               htr := THTMLTableRow.Create;
               //Allow user to set properties
               if Assigned(FOnDetailRowProperties) then begin
                   FOnDetailRowProperties(Self, htr);
               end;

               //Display each visible field
               for i := 0 to FieldCount - 1 do begin
                   if Fields[i].Visible then begin
                       htd := THTMLTableDetail.Create('');
                       if UseFieldAlign then begin //Check column alignment
                           htd.AlignHoriz := thAlignments[Fields[i].Alignment];
                       end;
                       //And display the field
                       if Fields[i] is TMemoField then begin //Add all the lines
                           slsMemo.Assign(TMemoField(Fields[i]));
                           htd.Text := EscapeText(slsMemo.Text);
                       end else begin //Add field as a link
                           if Fields[i] = LinkField then begin
                               htd.Add(THTMLAnchor.Create(LinkTarget.AsString, EscapeText(LinkField.DisplayText)))
                           end else begin  //Add text of field
                               htd.Text := EscapeText(Fields[i].DisplayText);
                           end;
                       end;
                       //Allow user to set properties
                       if Assigned(FOnCellProperties) then begin
                           FOnCellProperties(Self, Fields[i], htd);
                       end;
                       htr.Add(htd);
                   end;
               end;
               Add(htr);
               Next;
           end;

           Self.FDataLoaded:=True; //***Set non Empty Table flag

           GotoBookmark(bmkSave); //Return to original position
       finally
           FreeBookmark(bmkSave); //Free resources
           EnableControls; //Update screen again
           slsMemo.Free; //Release string list resources
       end;
   end;
end;

procedure THTMLDataTable.Clear;
//----------------------------------------------------------------------------------------------------------------------
begin
   Self.FDataLoaded:=False;
   inherited Clear;
end;

end.
