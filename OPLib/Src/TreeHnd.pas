{$IFDEF TreeHnd }
    {$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I OPLib.inc}

unit TreeHnd;

interface

uses
    Classes, SysUtils, Contnrs;

type
    TStrTree = Class ;
    TStrTreeNode = Class ;
    TNodeOperation = (noAdd, noRemove, noExtract, noUpdateState, noDestroying);
    TNodeEvent = procedure(Node : TStrTreeNode; Operation : TNodeOperation; var CanDo : boolean) of object;

    TStrTreeNode = Class (TObjectList)
    private
        FData : Pointer;
        FOnBeforeDestroyNode : TNodeEvent;
        FOnRemoveSubNode : TNodeEvent;
        FOnAddSubNode : TNodeEvent;
        FOnUpdateNode : TNodeEvent;
        FUpdateState : boolean;
        FOnExtractSubNode : TNodeEvent;
        function GetRootNode : TStrTreeNode;
        function GetSubNodes(Index : integer) : TStrTreeNode;
        procedure SetSubNodes(Index : integer; const Value : TStrTreeNode);
        procedure DoBeforeDestruction;
        procedure SetUpdateState(const Value : boolean);
    protected
        FParentNode : TStrTreeNode;
        FName : string;
        FUpdateCount : integer;
        property UpdateState : boolean read FUpdateState write SetUpdateState;
        procedure DoAddSubNode(SubNode : TStrTreeNode); virtual;
        procedure DoExtractSubNode(SubNode : TStrTreeNode); virtual;
        procedure DoUpdateNode(); virtual;
        procedure DoRemoveSubNode(SubNode : TStrTreeNode); virtual;
        function GetIsRoot : boolean; virtual;
        procedure Notify(Ptr : Pointer; Action : TListNotification); override;
        procedure SetName(const Value : string); virtual;
    public
        property Data : Pointer read FData write FData;
        property IsRoot : boolean read GetIsRoot;
        property OnAddSubNode : TNodeEvent read FOnAddSubNode write FOnAddSubNode;
        property OnExtractSubNode : TNodeEvent read FOnExtractSubNode write FOnExtractSubNode;
        property OnRemoveSubNode : TNodeEvent read FOnRemoveSubNode write FOnRemoveSubNode;
        property OnUpdateNode : TNodeEvent read FOnUpdateNode write FOnUpdateNode;
        property OnBeforeDestroyNode : TNodeEvent read FOnBeforeDestroyNode write FOnBeforeDestroyNode;
        property ParentNode : TStrTreeNode read FParentNode;
        property RootNode : TStrTreeNode read GetRootNode;
        property SubNodes[Index : Integer] : TStrTreeNode read GetSubNodes write SetSubNodes;
        property Name : string read FName write SetName;
        constructor Create(AParentNode : TStrTreeNode); overload; virtual;
        constructor Create(AParentNode : TStrTreeNode; const AName : string); overload; virtual;
        destructor Destroy; override;
        function Add(ANode : TStrTreeNode) : Integer;
        procedure BeginUpdate;
        procedure EndUpdate;
        function Extract(Item : TStrTreeNode) : TStrTreeNode;
        function FindChildNode(const ChildName : string; CaseSensitive : boolean = TRUE) : TStrTreeNode;
        function HasSubNodes : boolean;
        procedure BeforeDestruction; override;
    end;

    TStrTree = Class (TStrTreeNode);  //Funciona como no raiz

implementation

{ TStrTreeNode }

function TStrTreeNode.Add(ANode : TStrTreeNode) : Integer;
    //----------------------------------------------------------------------------------------------------------------------
var
    PreviousParent : TStrTreeNode;
begin
    PreviousParent := ANode.ParentNode;
    if Assigned(PreviousParent) then begin //Remover do pai anterior para alocar ao novo pai
        if PreviousParent <> Self then begin
            PreviousParent.Notify(ANode, lnExtracted);
        end else begin
            Result := Self.IndexOf(ANode);
            Exit; //Nada a fazer este no ja pertence a self
        end;
    end;
    ANode.FParentNode := Self;
    Result := inherited Add(ANode);
end;

procedure TStrTreeNode.BeforeDestruction;
//--------------------------------------------------------------------------------------------------------------------
//Realiza rotinas para ajustes dos nohs filhos
begin
    Self.DoBeforeDestruction;
    inherited;
end;

procedure TStrTreeNode.BeginUpdate;
//----------------------------------------------------------------------------------------------------------------------
begin
    Inc(Self.FUpdateCount);
    Self.UpdateState := FALSE;
end;

constructor TStrTreeNode.Create(AParentNode : TStrTreeNode);
    //----------------------------------------------------------------------------------------------------------------------
begin
    inherited Create(TRUE);
    if Assigned(AParentNode) then begin
        AParentNode.Add(Self);
    end else begin
        Self.FParentNode := AParentNode;
    end;
    Self.FUpdateState := TRUE;
    Self.FUpdateCount := 0;
end;

constructor TStrTreeNode.Create(AParentNode : TStrTreeNode; const AName : string);
    //----------------------------------------------------------------------------------------------------------------------------------
begin
    Self.FName := AName;
    Self.Create(AParentNode);
end;

procedure TStrTreeNode.DoAddSubNode(SubNode : TStrTreeNode);
//----------------------------------------------------------------------------------------------------------------------
var
    CanDo : boolean;
begin
    if Assigned(Self.FOnAddSubNode) then begin
        CanDo := TRUE;
        Self.FOnAddSubNode(SubNode, noAdd, CanDo);
        if not CanDo then begin
            Abort();
        end;
    end;
end;

procedure TStrTreeNode.DoBeforeDestruction;
//----------------------------------------------------------------------------------------------------------------------
var
    CanDo : Boolean;
    i : integer;
begin
    if Self.OwnsObjects then begin
        { TODO -oRoger -cDSG : Chamar destrutior dos filhos para operacao recursiva chamar os eventos na ordem inversa }
        while (Self.Count > 0) do begin
            Self.Last.Free;
        end;
        //        Self.Clear();  //Mata nos filhos
    end else begin
        for i := Self.Count - 1 downto 0 do begin
            { TODO -oRoger -cLIB : Trocar linha por chamada a Extract para todos os elementos }
            Self.Remove(Self.SubNodes[i]);
            Self.SubNodes[i].FParentNode := NIL;
        end;
    end;
    if Assigned(Self.FOnBeforeDestroyNode) then begin
        CanDo := TRUE;
        Self.FOnBeforeDestroyNode(Self, noDestroying, CanDo);
        if not CanDo then begin
            Abort();
        end;
    end;
end;

procedure TStrTreeNode.DoRemoveSubNode(SubNode : TStrTreeNode);
//----------------------------------------------------------------------------------------------------------------------
var
    CanDo : boolean;
begin
    if Assigned(Self.FOnRemoveSubNode) then begin
        CanDo := TRUE;
        Self.FOnRemoveSubNode(SubNode, noRemove, CanDo);
        if not CanDo then begin
            Abort();
        end;
    end;
end;

procedure TStrTreeNode.DoUpdateNode;
//----------------------------------------------------------------------------------------------------------------------
var
    CanDo : boolean;
begin
    if (Assigned(Self.FOnUpdateNode)) and (Self.FUpdateCount <= 0) then begin //Checa estado dos updates para permitir
        CanDo := TRUE;
        Self.FOnUpdateNode(Self, noUpdateState, CanDo);
        if not CanDo then begin
            Abort();
        end;
    end;
end;

procedure TStrTreeNode.EndUpdate;
//----------------------------------------------------------------------------------------------------------------------
begin
    Dec(Self.FUpdateCount);
    if Self.FUpdateCount = 0 then begin
        Self.UpdateState := TRUE;
    end;
end;

function TStrTreeNode.GetRootNode : TStrTreeNode;
    //----------------------------------------------------------------------------------------------------------------------
begin
    if Self.FParentNode = NIL then begin
        Result := Self;
    end else begin
        Result := Self.FParentNode.RootNode;
    end;
end;

function TStrTreeNode.GetSubNodes(Index : integer) : TStrTreeNode;
    //----------------------------------------------------------------------------------------------------------------------
begin
    Result := TStrTreeNode(Self.Items[Index]);
end;

function TStrTreeNode.HasSubNodes : boolean;
    //----------------------------------------------------------------------------------------------------------------------
begin
    Result := (Self.Count > 0);
end;

procedure TStrTreeNode.Notify(Ptr : Pointer; Action : TListNotification);
//----------------------------------------------------------------------------------------------------------------------
begin
    case Action of
        lnAdded    : begin
            Self.DoAddSubNode(Ptr);
        end;
        lnExtracted    : begin
            Self.DoExtractSubNode(Ptr);
            Self.Extract(Ptr);
        end;
        lnDeleted    : begin
            Self.DoRemoveSubNode(Ptr);
        end;
    end;
    inherited;
end;

procedure TStrTreeNode.SetSubNodes(Index : integer; const Value : TStrTreeNode);
//----------------------------------------------------------------------------------------------------------------------
begin
    Self.Items[Index] := Value;
end;

procedure TStrTreeNode.SetName(const Value : string);
//----------------------------------------------------------------------------------------------------------------------
var
    OldValue : string;
begin
    { TODO -oRoger -cLIB : Criar propriedade para checar possibilidade de nomes de irmaos comuns e levantar excesso para este caso }
    OldValue := FName;
    Self.FName := Value;
    try
        Self.DoUpdateNode;
    except
        Self.FName := OldValue;
        raise;
    end;
end;

procedure TStrTreeNode.SetUpdateState(const Value : boolean);
//----------------------------------------------------------------------------------------------------------------------
var
    i : integer;
begin
    FUpdateState := Value;
    if Value then begin
        Self.FUpdateCount := 0;
        for i := 0 to Self.Count - 1 do begin
            TStrTreeNode(Self.Items[i]).DoUpdateNode;
        end;
        Self.DoUpdateNode;
    end;
end;

procedure TStrTreeNode.DoExtractSubNode(SubNode : TStrTreeNode);
//----------------------------------------------------------------------------------------------------------------------
var
    CanDo : boolean;
begin
    if Assigned(Self.FOnExtractSubNode) then begin
        CanDo := TRUE;
        Self.FOnExtractSubNode(SubNode, noRemove, CanDo);
        if not CanDo then begin
            Abort();
        end;
    end;
end;

function TStrTreeNode.FindChildNode(const ChildName : string; CaseSensitive : boolean = TRUE) : TStrTreeNode;
    //----------------------------------------------------------------------------------------------------------------------------------
var
    i : integer;
begin
    Result := NIL;
    if CaseSensitive then begin
        for i := 0 to Self.Count - 1 do begin
            if Self.GetSubNodes(i).Name = ChildName then begin
                Result := Self.GetSubNodes(i);
                Break;
            end;
        end;
    end else begin
        for i := 0 to Self.Count - 1 do begin
            if SameText(Self.GetSubNodes(i).Name, ChildName) then begin
                Result := Self.GetSubNodes(i);
                Break;
            end;
        end;
    end;
end;

function TStrTreeNode.GetIsRoot : boolean;
    //----------------------------------------------------------------------------------------------------------------------------------
begin
    Result := (Self.FParentNode = NIL);
end;

function TStrTreeNode.Extract(Item : TStrTreeNode) : TStrTreeNode;
    //----------------------------------------------------------------------------------------------------------------------
    //Remover da lista sem destruir o elemento
begin
    Result := TStrTreeNode(inherited Extract(Item));
    if Item <> NIL then begin
        Item.FParentNode := NIL; //Remove referencia a si como pai deste noh
    end;
end;

destructor TStrTreeNode.Destroy;
    //----------------------------------------------------------------------------------------------------------------------
begin
    if Assigned(Self.FParentNode) then begin
        Self.FParentNode.Extract(Self);
    end;
    inherited;
end;

end.


