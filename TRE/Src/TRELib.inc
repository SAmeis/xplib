{$IFDEF TRELib}
	{$DEFINE DEBUG_UNIT }
{$ENDIF}

{$IFDEF DEBUG_ALL}
	{$DEFINE DEBUG_UNIT }
{$ENDIF}


//************** START COMMON PART **************

//****************************************************   RTTI  ********************************************************
{$DEFINE RTTI_LEVEL2} //valores possiveis RTTI_LEVEL0, RTTI_LEVEL1 e RTTI_LEVEL2
{$IFDEF RTTI_LEVEL2}
{$DEFINE RTTI_LEVEL1} //Setando nivel 2 implica em setar nivel 1(mais restrito)
{$ENDIF}
{$IFNDEF RTTI_LEVEL1}
{$DEFINE RTTI_LEVEL0} //Sem nível 1 implica sem RTTI alguma
{$ENDIF}

//Habilita total geração de RTTI para este código(habilitado apenas para casos necessários)
{$IFDEF RTTI_LEVEL0}
	//Desliga totalmente RTTI
	{$TYPEINFO OFF}
	{$METHODINFO OFF}
{$ELSE}
	//Habilita RTTI base
	{$IFDEF RTTI_LEVEL1}
		//={$M-}
		{$TYPEINFO ON}
	{$ENDIF}
	//Habilita RTTI completa, tem como requisito o nivel 1(typeinfo on)
	{$IFDEF RTTI_LEVEL2}
		{$METHODINFO ON}
	{$ENDIF}
{$ENDIF}
//****************************************************   RTTI  ********************************************************


//Alinhamento de 64 bits ( para os novos processadores e os atuais )
{$A8}

//Emulacao de ponto flutuante -> nunca mais!!!!
{$E-}

//== {$B-} desliga avaliacao completa ( se desejavel ligar localmente )
{$BOOLEVAL OFF}

//Force FAR calls ( Sem uso hoje )
{$F-}

// == {$G-} Disables creation of imported data references. increases memory-access efficiency,
//but prevents a packaged unit where it occurs from referencing variables in other packages.
{$IMPORTEDDATA OFF}

 // == {$H+} , Nao compensa o uso das shortStrings hoje, a menos para APIs
{$LONGSTRINGS ON}

 // == {$I+} Sempre checar por operacoes de I/O
{$IOCHECKS ON}

 // == {$J-}, Ora se eh constante eh constante
{$WRITEABLECONST OFF}

{$K+} // Smart callbacks ( Sem uso hoje )

{$N+} //N+ 80x87 instructions( obvio )

 //== {$P+} , Open string params e serve apenas para compatibilidade
{$OPENSTRINGS ON}

 // == {$Q-}, Deixado como desligado para aceleracao, se necessario ligar localmente
{$OVERFLOWCHECKS OFF}

 // {$R-}, Deixado como desligado para aceleracao, se necessario ligar localmente
{$RANGECHECKS OFF}

 // == {$S+} ( Estou meio em duvida a respeito disso )
{$STACKCHECKS ON}

 // == {$T+}, Ajuda a evitar erros
{$TYPEDADDRESS ON}

 // == {$U-}, Se comprou Pentium com bug -> jogue fora
{$SAFEDIVIDE OFF}

 // == {$V+}, Evitar confusao entre long e short strings
{$VARSTRINGCHECKS ON}

//Locals Symbols -> Sempre ativar, desconsiderando o tempo e o tamanho dos arquivos intermediarios, remover apenas para RELEASE definido
{$LOCALSYMBOLS ON} // == {$L+}

//Reference Info -> Sempre ativar, desconsiderando o tempo e o tamanho dos arquivos intermediarios, remover apenas para RELEASE definido
{$REFERENCEINFO ON} // == {$Y+}

// == {$W+}, Prevencao para uso de depuracao externa
{$STACKFRAMES ON}

 // {$X+}, Nao dah pra imaginar sem isso hoje
{$EXTENDEDSYNTAX ON}

{$Z2} //enumerated type has more than 256 values and/or stored as an unsigned word.( minha preferencia por performance de cpu )


//************** END COMMON PART **************



{--------------------------------------------------------------------------------------------------------------------------------}
//************** START DEBUG ONLY **************
{$IFDEF DEBUG_UNIT }
	 // == {$C+} Assertions habilitar para depuracao, remover caso contrario
	{$ASSERTIONS ON}

	  // = {$D+} Debug info, habilitar para depuracao, remover caso contrario
	{$DEBUGINFO ON}

	 // == {$O-}, Sempre remover para depuracao apenas
	{$OPTIMIZATION OFF}

	//Locals Symbols -> Sempre ativar, desconsiderando o tempo e o tamanho dos arquivos intermediarios, remover apenas para RELEASE definido
	{$LOCALSYMBOLS ON} // == {$L+}

	//Reference Info -> Sempre ativar, desconsiderando o tempo e o tamanho dos arquivos intermediarios, remover apenas para RELEASE definido
	{$REFERENCEINFO ON} // == {$Y+}

	 // == {$W+}, Prevencao para uso de depuracao externa
	{$STACKFRAMES ON}
{$ELSE}
	 // == {$C+} Assertions habilitar para depuracao, remover caso contrario
	{$ASSERTIONS OFF}

	  // = {$D+} Debug info, habilitar para depuracao, remover caso contrario
	{$DEBUGINFO OFF}

	 // == {$O-}, Sempre remover para depuracao apenas
	{$OPTIMIZATION ON}

{$ENDIF}
//************** END DEBUG ONLY **************


{--------------------------------------------------------------------------------------------------------------------------------}
//ESTAS DIRETIVAS SERAO USADAS SEMPRE PARA O CASO DE RISCO DE ENGENHARIA REVERSA
//************** START RELEASE ONLY **************
{$IFDEF RELEASE}
	 // == {$O+}, deixado como padrao removido para depuracao apenas
	{$OPTIMIZATION ON}

	//Locals Symbols -> Sempre ativar, desconsiderando o tempo e o tamanho dos arquivos intermediarios, remover apenas para RELEASE definido
	{$LOCALSYMBOLS OFF} // == {$L-}

	//Reference Info -> Sempre ativar, desconsiderando o tempo e o tamanho dos arquivos intermediarios, remover apenas para RELEASE definido
	{$REFERENCEINFO OFF} // == {$Y-}

	 // == {$W-}, Ligar apenas para depuracao remota com ferramenta externa
	{$STACKFRAMES OFF}
{$ENDIF}

//************** END RELEASE ONLY **************




//LINKER
{$MINSTACKSIZE $00004000}
{$MAXSTACKSIZE $00100000}
{$IMAGEBASE $00400000}
{$APPTYPE GUI}


//WARNs
{$WARN SYMBOL_DEPRECATED ON}
{$WARN SYMBOL_LIBRARY ON}
{$WARN SYMBOL_PLATFORM ON}
{$WARN SYMBOL_EXPERIMENTAL ON}
{$WARN UNIT_LIBRARY ON}
{$WARN UNIT_PLATFORM ON}
{$WARN UNIT_DEPRECATED ON}
{$WARN UNIT_EXPERIMENTAL ON}
{$WARN HRESULT_COMPAT ON}
{$WARN HIDING_MEMBER ON}
{$WARN HIDDEN_VIRTUAL ON}
{$WARN GARBAGE ON}
{$WARN BOUNDS_ERROR ON}
{$WARN ZERO_NIL_COMPAT ON}
{$WARN STRING_CONST_TRUNCED ON}
{$WARN FOR_LOOP_VAR_VARPAR ON}
{$WARN TYPED_CONST_VARPAR ON}
{$WARN ASG_TO_TYPED_CONST ON}
{$WARN CASE_LABEL_RANGE ON}
{$WARN FOR_VARIABLE ON}
{$WARN CONSTRUCTING_ABSTRACT ON}
{$WARN COMPARISON_FALSE ON}
{$WARN COMPARISON_TRUE ON}
{$WARN COMPARING_SIGNED_UNSIGNED ON}
{$WARN COMBINING_SIGNED_UNSIGNED ON}
{$WARN UNSUPPORTED_CONSTRUCT ON}
{$WARN FILE_OPEN ON}
{$WARN FILE_OPEN_UNITSRC ON}
{$WARN BAD_GLOBAL_SYMBOL ON}
{$WARN DUPLICATE_CTOR_DTOR ON}
{$WARN INVALID_DIRECTIVE ON}
{$WARN PACKAGE_NO_LINK ON}
{$WARN PACKAGED_THREADVAR ON}
{$WARN IMPLICIT_IMPORT ON}
{$WARN HPPEMIT_IGNORED ON}
{$WARN NO_RETVAL ON}
{$WARN USE_BEFORE_DEF ON}
{$WARN FOR_LOOP_VAR_UNDEF ON}
{$WARN UNIT_NAME_MISMATCH ON}
{$WARN NO_CFG_FILE_FOUND ON}
{$WARN IMPLICIT_VARIANTS ON}
{$WARN UNICODE_TO_LOCALE ON}
{$WARN LOCALE_TO_UNICODE ON}
{$WARN IMAGEBASE_MULTIPLE ON}
{$WARN SUSPICIOUS_TYPECAST ON}
{$WARN PRIVATE_PROPACCESSOR ON}
{$WARN UNSAFE_TYPE ON}
{$WARN UNSAFE_CODE ON}
{$WARN UNSAFE_CAST ON}
{$WARN OPTION_TRUNCATED ON}
{$WARN WIDECHAR_REDUCED ON}
{$WARN DUPLICATES_IGNORED ON}
{$WARN UNIT_INIT_SEQ ON}
{$WARN LOCAL_PINVOKE ON}
{$WARN MESSAGE_DIRECTIVE ON}
{$WARN TYPEINFO_IMPLICITLY_ADDED ON}
{$WARN RLINK_WARNING ON}
{$WARN IMPLICIT_STRING_CAST ON}
{$WARN IMPLICIT_STRING_CAST_LOSS ON}
{$WARN EXPLICIT_STRING_CAST ON}
{$WARN EXPLICIT_STRING_CAST_LOSS ON}
{$WARN CVT_WCHAR_TO_ACHAR ON}
{$WARN CVT_NARROWING_STRING_LOST ON}
{$WARN CVT_ACHAR_TO_WCHAR ON}
{$WARN CVT_WIDENING_STRING_LOST ON}
{$WARN XML_WHITESPACE_NOT_ALLOWED ON}
{$WARN XML_UNKNOWN_ENTITY ON}
{$WARN XML_INVALID_NAME_START ON}
{$WARN XML_INVALID_NAME ON}
{$WARN XML_EXPECTED_CHARACTER ON}
{$WARN XML_CREF_NO_RESOLVE ON}
{$WARN XML_NO_PARM ON}
{$WARN XML_NO_MATCHING_PARM ON}

{$IFDEF VER270} //Delphi XE6
	{$WARN NON_PORTABLE_TYPECAST ON}
	{$WARN IMMUTABLE_STRINGS ON}
{$ENDIF}

