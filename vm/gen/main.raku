#use Grammar::Debugger;

grammar StarBCD::Grammar {
	token TOP {^\s*
		types \h+ '{'\s*
			<type>**26 %% [\s+]
		'}' \s+
		<decl>+ % [\s+]
	\s*$}

	# {dd $/.orig.substr($/.from)}
	regex ident { [ <[a..z]> | _ <[\w']> ] <[\w']>* }
	token label { <[a..z_]> <[\w']>* ':' }
	token typename { <[A..Z]> <[\w']>* }
	token type { <.typename>+ % '.' }
	token id { \d+ }

	token sint { '-'? \d+ }
	token uint { \d+ }
	token dec { '-'? \d+ '.' \d+ }
	token string {
		'"'
		$<contents> = (
			| $<chars> = <-[\\"]>+
			| '\\' $<esc> = (<.xdigit> ** 2)
		)*
		'"'
	}


	token parents {
		\h+ of \h+ '#['\s* <typeref>+ % [\s+] \s*']'
	}

	token typevars {
		\h+ typevars \h+ '{'\s*
			<typevar-decl>+ % [\s+]
		\s*'}'
	}

	token typevar-decl {
		type \h+ <id> \h+ <typename> <parents>?
	}
	

	proto token decl {*}

	token decl:sym<category> {
		{} <sym> \h+ <id> \h+ $<name> = (<type> '+' <type>) \h+ <path-type=type> \h+ for \h+ <for-type=type> <typevars>? \h+ '{'
			<static-single-methods>?
			<static-multi-methods>?

			<static-members>?

			<static-init>?

			<single-inits>?
			<multi-inits>?

			<inst-single-methods>?
			<inst-multi-methods>?
			<inst-cast-methods>?
			<binary-methods>?
			<unary-methods>?

			<static-deinit>?
		\s*'}'
	}

	token decl:sym<opaquetype> {
		{} <sym> \h+ <id> \h+ <type> <typevars>? \h+ '{'
			<static-single-methods>?
			<static-multi-methods>?

			<inst-single-methods>?
			<inst-multi-methods>?
			<inst-cast-methods>?
			<binary-methods>?
			<unary-methods>?
		\s*'}'
	}

	token decl:sym<newtype> {
		{} <sym> \h+ <id> \h+ <type> \h+ '(' <base=typeref> ')' $<is-noinherit> = (\h+ is \h+ noinherit)? <typevars>? \h+ '{'
			<static-single-methods>?
			<static-multi-methods>?

			<static-members>?

			<static-init>?

			<inst-single-methods>?
			<inst-multi-methods>?
			<inst-cast-methods>?
			<binary-methods>?
			<unary-methods>?

			<static-deinit>?
		\s*'}'
	}

	token decl:sym<module> {
		{} <sym> \h+ <id> \h+ <type> <parents>? $<is-main> = (\h+ is \h+ main \h+ <id>)? <typevars>? \h+ '{'
			<static-single-methods>?
			<static-multi-methods>?
			
			<static-members>?

			<static-init>?

			<inst-single-methods>?
			<inst-multi-methods>?
			<inst-cast-methods>?
			<binary-methods>?
			<unary-methods>?

			<static-deinit>?
		\s*'}'
	}

	token decl:sym<class> {
		{} <sym> \h+ <id> \h+ <type> <parents>? <typevars>? \h+ '{'
			<static-single-methods>?
			<static-multi-methods>?
			
			<static-members>?
			<inst-members>?

			<static-init>?
			<default-init>?

			<single-inits>?
			<multi-inits>?

			<inst-single-methods>?
			<inst-multi-methods>?
			<inst-cast-methods>?
			<binary-methods>?
			<unary-methods>?

			<inst-single-method-vtable>?
			<inst-multi-method-vtable>?
			<inst-cast-method-vtable>?
			<binary-method-vtable>?
			<unary-method-vtable>?

			<deinit>?
			<static-deinit>?
		\s*'}'
	}

	token decl:sym<protocol> {
		{} <sym> \h+ <id> \h+ <type> <parents>? <typevars>? \h+ '{'
			<static-single-methods>?
			<static-multi-methods>?

			<static-members>?
			<inst-members>?

			<static-init>?
			<default-init>?

			<single-inits>?
			<multi-inits>?

			<inst-single-methods>?
			<inst-multi-methods>?
			<inst-cast-methods>?
			<binary-methods>?
			<unary-methods>?

			<inst-single-method-vtable>?
			<inst-multi-method-vtable>?
			<inst-cast-method-vtable>?
			<binary-method-vtable>?
			<unary-method-vtable>?

			<deinit>?
			<static-deinit>?
		\s*'}'
	}

	token decl:sym<tagged-kind> {
		{} <sym> \h+ <id> \h+ <type> <parents>? $<is-flags> = (\h+ is \h+ flags)? <typevars>? \h+ '{'
			<static-single-methods>?
			<static-multi-methods>?

			<static-members>?
			<inst-members>?

			<tagged-cases>?

			<static-init>?
			<default-init>?

			<inst-single-methods>?
			<inst-multi-methods>?
			<inst-cast-methods>?
			<binary-methods>?
			<unary-methods>?

			<inst-single-method-vtable>?
			<inst-multi-method-vtable>?
			<inst-cast-method-vtable>?
			<binary-method-vtable>?
			<unary-method-vtable>?

			<deinit>?
			<static-deinit>?
		\s*'}'
	}

	token decl:sym<value-kind> {
		{} <sym> \h+ <id> \h+ <type> [\h+ '(' <base=type> ')']? <parents>? $<is-flags> = (\h+ is \h+ flags)? <typevars>? \h+ '{'
			<static-single-methods>?
			<static-multi-methods>?

			<static-members>?

			<value-cases>?

			<static-init>?

			<inst-single-methods>?
			<inst-multi-methods>?
			<inst-cast-methods>?
			<binary-methods>?
			<unary-methods>?

			<inst-single-method-vtable>?
			<inst-multi-method-vtable>?
			<inst-cast-method-vtable>?
			<binary-method-vtable>?
			<unary-method-vtable>?

			<deinit>?
			<static-deinit>?
		\s*'}'
	}


	token single-inits {
		\s+ 'single-inits' \h+ '{'\s*
			<single-init>* % [\s+]
		\s*'}'
	}

	token multi-inits {
		\s+ 'multi-inits' \h+ '{'\s*
			<multi-init>* % [\s+]
		\s*'}'
	}

	token static-single-methods {
		\s+ 'static-single-methods' \h+ '{'\s*
			<single-method>* % [\s+]
		\s*'}'
	}

	token static-multi-methods {
		\s+ 'static-multi-methods' \h+ '{'\s*
			<multi-method>* % [\s+]
		\s*'}'
	}

	token inst-single-methods {
		\s+ 'inst-single-methods' \h+ '{'\s*
			<single-method>* % [\s+]
		\s*'}'
	}

	token inst-multi-methods {
		\s+ 'inst-multi-methods' \h+ '{'\s*
			<multi-method>* % [\s+]
		\s*'}'
	}
	
	token inst-cast-methods {
		\s+ 'inst-cast-methods' \h+ '{'\s*
			<cast-method>* % [\s+]
		\s*'}'
	}

	token binary-methods {
		\s+ 'binary-methods' \h+ '{'\s*
			<binary-method>* % [\s+]
		\s*'}'
	}

	token unary-methods {
		\s+ 'unary-methods' \h+ '{'\s*
			<unary-method>* % [\s+]
		\s*'}'
	}

	token inst-single-method-vtable {
		\s+ 'inst-single-method-vtable' \h+ '{'\s*
			$<entries> = (
				'for' \h+ <typeref> \h+ '{'\s*
					<inst-single-method>* % [\s+]
				\s*'}'
			)* % [\s+]
		\s*'}'
	}

	token inst-multi-method-vtable {
		'inst-multi-method-vtable' \h+ '{'\s*
			$<entries> = (
				'for' \h+ <typeref> \h+ '{'\s*
					<inst-multi-method>* % [\s+]
				\s*'}'
			)* % [\s+]
		\s*'}'
	}

	token inst-cast-method-vtable {
		\s+ 'inst-cast-method-vtable' \h+ '{'\s*
			$<entries> = (
				'for' \h+ <typeref> \h+ '{'\s*
					<inst-cast-method>* % [\s+]
				\s*'}'
			)* % [\s+]
		\s*'}'
	}

	token binary-method-vtable {
		'binary-method-vtable' \h+ '{'\s*
			$<entries> = (
				'for' \h+ <typeref> \h+ '{'\s*
					<binary-method>* % [\s+]
				\s*'}'
			)* % [\s+]
		\s*'}'
	}

	token unary-method-vtable {
		\s+ 'unary-method-vtable' \h+ '{'\s*
			$<entries> = (
				'for' \h+ <typeref> \h+ '{'\s*
					<unary-method>* % [\s+]
				\s*'}'
			)* % [\s+]
		\s*'}'
	}

	token static-init {
		\s+ 'static-init' \h+ <opcodes>
	}

	token default-init {
		\s+ 'default-init' \h+ <opcodes>
	}

	token deinit {
		\s+ 'deinit' \h+ <opcodes>
	}

	token static-deinit {
		\s+ 'static-deinit' \h+ <opcodes>
	}
	
	token static-members {
		\s+ 'static-members' \h+ '{'\s*
			<member>* % [\s+]
		\s*'}'
	}

	token inst-members {
		\s+ 'inst-members' \h+ '{'\s*
			<member>* % [\s+]
		\s*'}'
	}

	token tagged-cases {
		\s+ 'tagged-cases' \h+ '{'\s*
			<tagged-case>* % [\s+]
		\s*'}'
	}

	token value-cases {
		\s+ 'value-cases' \h+ '{'\s*
			<value-case>* % [\s+]
		\s*'}'
	}


	token member {
		my \h+ <id> \h+ <ident> \h+ '(' <typeref> ')'
	}

	token tagged-case {
		has \h+ <id> \h+
		'['\s* [
			| $<slots> = (<label> \h+ '(' <typeref> ')')+ % [\s+]
			| <ident>
		] \s*']'
		[\h+ <init=opcodes>]?
	}

	token value-case {
		has \h+ <id> \h+ <ident>
		[\h+ <init=opcodes>]?
	}


	token single-sig {
		'['\s* <ident> \s*']'
	}
	
	token multi-sig {
		'['\s*
			$<params> = (<label> \h+ <id> \h+ '(' <typeref> ')')+ % [\s+]
		\s*']'
	}

	token ret {
		\h+ '(' <typeref> ')'
	}

	token single-init {
		init \h+ <id> \h+ <single-sig> \h+ <opcodes>
	}

	token multi-init {
		init \h+ <id> \h+ <multi-sig> <typevars>? \h+ <opcodes>
	}

	token single-method {
		on \h+ <id> \h+ <single-sig> <ret> \h+ <opcodes>
	}

	token multi-method {
		on \h+ <id> \h+ <multi-sig> <ret> <typevars>? \h+ <opcodes>
	}

	token cast-method {
		on \h+ <id> \h+ '['\s* <typeref> \s*']' <ret>? <typevars>? \h+ <opcodes>
	}

	token binary-method {
		operator \h+ <id> \h+ '`' $<name> = <-[`]>+ '`' \h+
		'['\s* $<param> = (<id> \h+ '(' <typeref> ')') \s*']' <ret> <typevars>? \h+ <opcodes>
	}

	token unary-method {
		operator \h+ <id> \h+ '`' $<name> = <-[`]>+ '`' <ret> \h+ <opcodes>
	}


	proto token typeref {*}
	
	token typeref:sym<decl> {
		<sym> '#' <id>
	}

	token typeref:sym<inst> {
		<sym> '#' <id> <type-inst-ctx>
	}

	token typeref:sym<typevar> {
		<typevar>
	}

	token typeref:sym<this> {
		<sym>
	}

	token typevar-id { \d+ }

	token type-inst-ctx {
		'{'\s*
		$<entries> = ('dvar#'<typevar-id> \h+ '=>' \h+ <typeref>)+ % [\s+]
		\s*'}'
	}

	proto token typevar {*}
	token typevar:sym<dvar> { <sym> '#' <typevar-id> }
	token typevar:sym<mvar> { <sym> '#' <typevar-id> }


	token typevar-inst-ctx {
		'{'\s*
		$<entries> = (
			<typevar> \h+ '=>' \h+ <typeref> $<mappings> = (\h+ '{'\s*
				# ...
			\s*'}')?
		)+ % [\s+]
		\s*'}'
	}

	
	token local-id { \d+ }
	token field-id { \d+ }
	token loop-id { \d+ }
	token kind-tag { \d+ }
	token member-id { \d+ }
	token init-id { \d+ }
	token method-id { \d+ }

	token opcodes {
		'{'\s*
		<opcode>* % [\s+]
		\s*'}'
	}

	proto token opcode {*}

	token opcode:sym<new-local>        { <sym> }
	token opcode:sym<get-local>        { <sym>\h+ <local-id> }
	token opcode:sym<set-local>        { <sym>\h+ <local-id> }
	token opcode:sym<tee-local>        { <sym>\h+ <local-id> }
	token opcode:sym<get-field>        { <sym>\h+ <field-id> }
	token opcode:sym<set-field>        { <sym>\h+ <field-id> }
	token opcode:sym<tee-field>        { <sym>\h+ <field-id> }
	token opcode:sym<get-static-field> { <sym>\h+ <field-id> }
	token opcode:sym<set-static-field> { <sym>\h+ <field-id> }
	token opcode:sym<tee-static-field> { <sym>\h+ <field-id> }

	token opcode:sym<dup>  { <sym> }
	token opcode:sym<dup2> { <sym> }
	token opcode:sym<swap> { <sym> }
	token opcode:sym<pop>  { <sym> }

	token opcode:sym<if-else> { 'if'\h+ <opcodes> [\h+ else \h+ <opcodes>]? }
	token opcode:sym<ifnot>   { <sym>\h+ <opcodes> }
	token opcode:sym<do>      { <sym>\h+ <loop-id> \h+ <opcodes> }
	token opcode:sym<loop>    { <sym>\h+ <loop-id> \h+ <opcodes> [\h+ then \h+ <opcodes>]? }
	token opcode:sym<try>     { <sym>\h+ <opcodes> \h+ catch \h+ <opcodes> }
	token opcode:sym<ret>     { <sym> }
	token opcode:sym<retvoid> { <sym> }
	token opcode:sym<throw>   { <sym>\h+ <string> }
	token opcode:sym<rethrow> { <sym> }
	token opcode:sym<break>   { <sym>\h+ <loop-id> }
	token opcode:sym<next>    { <sym>\h+ <loop-id> }

	token opcode:sym<native> { <sym>\h+ <ident> }

	token opcode:sym<int8>    { <sym>\h+ <sint> }
	token opcode:sym<uint8>   { <sym>\h+ <uint> }
	token opcode:sym<int16>   { <sym>\h+ <sint> }
	token opcode:sym<uint16>  { <sym>\h+ <uint> }
	token opcode:sym<int32>   { <sym>\h+ <sint> }
	token opcode:sym<uint32>  { <sym>\h+ <uint> }
	token opcode:sym<int64>   { <sym>\h+ <sint> }
	token opcode:sym<uint64>  { <sym>\h+ <uint> }
	token opcode:sym<float32> { <sym>\h+ <dec> }
	token opcode:sym<float64> { <sym>\h+ <dec> }
	token opcode:sym<dec64>   { <sym>\h+ <dec> }
	token opcode:sym<char>    { <sym>\h+ <uint> }
	token opcode:sym<str>     { <sym>\h+ <string> }
	token opcode:sym<true>    { <sym> }
	token opcode:sym<false>   { <sym> }
	token opcode:sym<this>    { <sym> }

	token opcode:sym<block> { <sym>\h+ <opcodes> }

	token opcode:sym<vcase-id>      { <sym>\h+ <typeref> \h+ <kind-tag> }
	token opcode:sym<tcase-id>      { <sym>\h+ <typeref> \h+ <kind-tag> }
	token opcode:sym<kind-id>       { <sym> }
	token opcode:sym<kind-slot>     { <sym>\h+ <slot=id> }
	token opcode:sym<kind-value>    { <sym> }
	token opcode:sym<upcast>        { <sym>\h+ <typeref> }
	token opcode:sym<downcast>      { <sym>\h+ <typeref> }
	token opcode:sym<native-cast>   { <sym>\h+ <typeref> }
	token opcode:sym<dynamic-cast>  { <sym>\h+ <typeref> }
	token opcode:sym<of-type>       { <sym>\h+ <typeref> }
	token opcode:sym<new-ptr>       { <sym>\h+ <typeref> }
	token opcode:sym<ptr-from-addr> { <sym>\h+ <typeref> }

	token opcode:sym<get-member>        { <sym>\h+ <member-id> }
	token opcode:sym<set-member>        { <sym>\h+ <member-id> }
	token opcode:sym<tee-member>        { <sym>\h+ <member-id> }
	token opcode:sym<get-static-member> { <sym>\h+ <typeref> \h+ <member-id> }
	token opcode:sym<set-static-member> { <sym>\h+ <typeref> \h+ <member-id> }
	token opcode:sym<tee-static-member> { <sym>\h+ <typeref> \h+ <member-id> }

	token opcode:sym<default-init> { <sym>\h+ <typeref> }
	token opcode:sym<init-this-s>  { <sym>\h+ <typeref> \h+ <init-id> }
	token opcode:sym<init-this-m>  { <sym>\h+ <typeref> \h+ <init-id> [\h* <typevar-inst-ctx>]? }
	token opcode:sym<send-is>      { <sym>\h+ <typeref> \h+ <init-id> }
	token opcode:sym<send-im>      { <sym>\h+ <typeref> \h+ <init-id> [\h* <typevar-inst-ctx>]? }
	token opcode:sym<send-ss>      { <sym>\h+ <typeref> \h+ <method-id> }
	token opcode:sym<send-ms>      { <sym>\h+ <typeref> \h+ <method-id> [\h* <typevar-inst-ctx>]? }
	token opcode:sym<send-si>      { <sym>\h+ <typeref> \h+ <method-id> }
	token opcode:sym<send-dyn-si>  { <sym>\h+ <typeref> \h+ <method-id> }
	token opcode:sym<send-mi>      { <sym>\h+ <typeref> \h+ <method-id> [\h* <typevar-inst-ctx>]? }
	token opcode:sym<send-dyn-mi>  { <sym>\h+ <typeref> \h+ <method-id> [\h* <typevar-inst-ctx>]? }
	token opcode:sym<send-c>       { <sym>\h+ <typeref> \h+ <method-id> [\h* <typevar-inst-ctx>]? }
	token opcode:sym<send-dyn-c>   { <sym>\h+ <typeref> \h+ <method-id> [\h* <typevar-inst-ctx>]? }
	token opcode:sym<send-bo>      { <sym>\h+ <typeref> \h+ <method-id> [\h* <typevar-inst-ctx>]? }
	token opcode:sym<send-dyn-bo>  { <sym>\h+ <typeref> \h+ <method-id> [\h* <typevar-inst-ctx>]? }
	token opcode:sym<send-uo>      { <sym>\h+ <typeref> \h+ <method-id> }
	token opcode:sym<send-dyn-uo>  { <sym>\h+ <typeref> \h+ <method-id> }

	token opcode:sym<init-class>       { <sym>\h+ <typeref> }
	token opcode:sym<init-tkind>       { <sym>\h+ <typeref> \h+ <kind-tag> }
	token opcode:sym<init-vkind>       { <sym>\h+ <typeref> \h+ <kind-tag> }
	token opcode:sym<init-multi-tkind> { <sym>\h+ <typeref> \h+ <kind-tag> }
	token opcode:sym<init-multi-vkind> { <sym>\h+ <typeref> \h+ <kind-tag> }

	token opcode:sym<multi-kind-has-tag>  { <sym>\h+ <kind-tag> }
	token opcode:sym<multi-kind-get-tag>  { <sym>\h+ <kind-tag> }
	token opcode:sym<multi-kind-get-slot> { <sym>\h+ <kind-tag> \h+ <slot=id> }
}

enum Opcode <<
	#| Storage / Access
	O_NEW_LOCAL O_GET_LOCAL O_SET_LOCAL O_TEE_LOCAL
	O_GET_FIELD O_SET_FIELD O_TEE_FIELD
	O_GET_STATIC_FIELD O_SET_STATIC_FIELD O_TEE_STATIC_FIELD

	#| Stack manip
	O_DUP O_DUP2 O_SWAP O_POP

	#| Control flow
	O_IF O_IF_NOT O_IF_ELSE
	O_DO
	O_LOOP
	O_TRY    
	O_RET O_RET_VOID
	O_THROW O_RETHROW
	O_BREAK
	O_NEXT

	#| Natives
	O_NATIVE

	#| Values
	O_INT8 O_UINT8 O_INT16 O_UINT16 O_INT32 O_UINT32 O_INT64 O_UINT64
	O_FLOAT32 O_FLOAT64 O_DEC64
	O_CHAR
	O_STR
	O_TRUE O_FALSE
	O_THIS

	#| Comprehension
	O_BLOCK

	#| Operations
	O_VCASE_ID O_TCASE_ID
	O_KIND_ID O_KIND_SLOT O_KIND_VALUE
	O_UPCAST O_DOWNCAST O_NATIVE_CAST O_DYNAMIC_CAST
	O_OF_TYPE
	O_NEW_PTR O_PTR_FROM_ADDR

	#| Members
	O_GET_MEMBER O_SET_MEMBER O_TEE_MEMBER
	O_GET_STATIC_MEMBER O_SET_STATIC_MEMBER O_TEE_STATIC_MEMBER

	#| Messaging
	O_DEFAULT_INIT
	O_INIT_THIS_S O_INIT_THIS_M
	O_SEND_IS
	O_SEND_IM
	O_SEND_SS
	O_SEND_MS
	O_SEND_SI O_SEND_DYN_SI
	O_SEND_MI O_SEND_DYN_MI
	O_SEND_C O_SEND_DYN_C
	O_SEND_BO O_SEND_DYN_BO
	O_SEND_UO O_SEND_DYN_UO

	#| Creation
	O_INIT_CLASS
	O_INIT_TKIND O_INIT_VKIND
	O_INIT_MULTI_TKIND O_INIT_MULTI_VKIND

	#| Multi kinds
	O_MULTI_KIND_HAS_TAG O_MULTI_KIND_GET_TAG
	O_MULTI_KIND_GET_SLOT
>>;

enum NativeOp <<
	# all values
	value_eq
	value_new
	cast_value_str
	value_address
	
	# multi-kinds
	multikind_truthy
	multikind_has
	multikind_include
	multikind_exclude

	# bools
	bool_not
	bool_and
	bool_or
	bool_xor
	bool_eq
	bool_ne
	cast_bool_i8
	cast_bool_u8
	cast_bool_i16
	cast_bool_u16
	cast_bool_i32
	cast_bool_u32
	cast_bool_i64
	cast_bool_u64

	# uint8
	u8_truthy
	u8_compl
	u8_succ
	u8_pred
	u8_add
	u8_sub
	u8_mult
	u8_pow
	u8_div
	u8_idiv
	u8_mod
	u8_mod0
	u8_and
	u8_or
	u8_xor
	u8_shl
	u8_shr
	u8_eq
	u8_ne
	u8_gt
	u8_ge
	u8_lt
	u8_le
	cast_u8_bool
	cast_u8_i8
	cast_u8_i16
	cast_u8_u16
	cast_u8_i32
	cast_u8_u32
	cast_u8_i64
	cast_u8_u64
	cast_u8_f32
	cast_u8_f64
	cast_u8_d64
	cast_u8_str

	# int8
	i8_truthy
	i8_abs
	i8_neg
	i8_compl
	i8_succ
	i8_pred
	i8_add
	i8_sub
	i8_mult
	i8_pow
	i8_div
	i8_idiv
	i8_mod
	i8_mod0
	i8_and
	i8_or
	i8_xor
	i8_shl
	i8_shr
	i8_eq
	i8_ne
	i8_gt
	i8_ge
	i8_lt
	i8_le
	cast_i8_bool
	cast_i8_u8
	cast_i8_i16
	cast_i8_u16
	cast_i8_i32
	cast_i8_u32
	cast_i8_i64
	cast_i8_u64
	cast_i8_f32
	cast_i8_f64
	cast_i8_d64
	cast_i8_str

	# uint16
	u16_truthy
	u16_compl
	u16_succ
	u16_pred
	u16_add
	u16_sub
	u16_mult
	u16_pow
	u16_div
	u16_idiv
	u16_mod
	u16_mod0
	u16_and
	u16_or
	u16_xor
	u16_shl
	u16_shr
	u16_eq
	u16_ne
	u16_gt
	u16_ge
	u16_lt
	u16_le
	cast_u16_bool
	cast_u16_i8
	cast_u16_u8
	cast_u16_i16
	cast_u16_i32
	cast_u16_u32
	cast_u16_i64
	cast_u16_u64
	cast_u16_f32
	cast_u16_f64
	cast_u16_d64
	cast_u16_str

	# int16
	i16_truthy
	i16_abs
	i16_neg
	i16_compl
	i16_succ
	i16_pred
	i16_add
	i16_sub
	i16_mult
	i16_pow
	i16_div
	i16_idiv
	i16_mod
	i16_mod0
	i16_and
	i16_or
	i16_xor
	i16_shl
	i16_shr
	i16_eq
	i16_ne
	i16_gt
	i16_ge
	i16_lt
	i16_le
	cast_i16_bool
	cast_i16_i8
	cast_i16_u8
	cast_i16_u16
	cast_i16_i32
	cast_i16_u32
	cast_i16_i64
	cast_i16_u64
	cast_i16_f32
	cast_i16_f64
	cast_i16_d64
	cast_i16_str

	# uint32
	u32_truthy
	u32_compl
	u32_succ
	u32_pred
	u32_add
	u32_sub
	u32_mult
	u32_pow
	u32_div
	u32_idiv
	u32_mod
	u32_mod0
	u32_and
	u32_or
	u32_xor
	u32_shl
	u32_shr
	u32_eq
	u32_ne
	u32_gt
	u32_ge
	u32_lt
	u32_le
	cast_u32_bool
	cast_u32_i8
	cast_u32_u8
	cast_u32_i16
	cast_u32_u16
	cast_u32_i32
	cast_u32_i64
	cast_u32_u64
	cast_u32_f32
	cast_u32_f64
	cast_u32_d64
	cast_u32_str

	# int32
	i32_truthy
	i32_abs
	i32_neg
	i32_compl
	i32_succ
	i32_pred
	i32_add
	i32_sub
	i32_mult
	i32_pow
	i32_div
	i32_idiv
	i32_mod
	i32_mod0
	i32_and
	i32_or
	i32_xor
	i32_shl
	i32_shr
	i32_eq
	i32_ne
	i32_gt
	i32_ge
	i32_lt
	i32_le
	cast_i32_bool
	cast_i32_i8
	cast_i32_u8
	cast_i32_i16
	cast_i32_u16
	cast_i32_u32
	cast_i32_i64
	cast_i32_u64
	cast_i32_f32
	cast_i32_f64
	cast_i32_d64
	cast_i32_str

	# uint64
	u64_truthy
	u64_compl
	u64_succ
	u64_pred
	u64_add
	u64_sub
	u64_mult
	u64_pow
	u64_div
	u64_idiv
	u64_mod
	u64_mod0
	u64_and
	u64_or
	u64_xor
	u64_shl
	u64_shr
	u64_eq
	u64_ne
	u64_gt
	u64_ge
	u64_lt
	u64_le
	cast_u64_bool
	cast_u64_i8
	cast_u64_u8
	cast_u64_i16
	cast_u64_u16
	cast_u64_i32
	cast_u64_u32
	cast_u64_i64
	cast_u64_f32
	cast_u64_f64
	cast_u64_d64
	cast_u64_str

	# int64
	i64_truthy
	i64_abs
	i64_neg
	i64_compl
	i64_succ
	i64_pred
	i64_add
	i64_sub
	i64_mult
	i64_pow
	i64_div
	i64_idiv
	i64_mod
	i64_mod0
	i64_and
	i64_or
	i64_xor
	i64_shl
	i64_shr
	i64_eq
	i64_ne
	i64_gt
	i64_ge
	i64_lt
	i64_le
	cast_i64_bool
	cast_i64_i8
	cast_i64_u8
	cast_i64_i16
	cast_i64_u16
	cast_i64_i32
	cast_i64_u32
	cast_i64_u64
	cast_i64_f32
	cast_i64_f64
	cast_i64_d64
	cast_i64_str

	# float32
	f32_nan
	f32_inf
	f32_neg_inf
	f32_pi
	f32_e
	f32_is_nan
	f32_sign
	f32_abs
	f32_sqrt
	f32_exp
	f32_sin
	f32_cos
	f32_tan
	f32_asin
	f32_acos
	f32_atan
	f32_floor
	f32_ceil
	f32_trunc
	f32_round
	f32_ln
	f32_log
	f32_truthy
	f32_neg
	f32_succ
	f32_pred
	f32_add
	f32_sub
	f32_mult
	f32_pow
	f32_div
	f32_idiv
	f32_mod
	f32_mod0
	f32_eq
	f32_ne
	f32_gt
	f32_ge
	f32_lt
	f32_le
	cast_f32_i8
	cast_f32_u8
	cast_f32_i16
	cast_f32_u16
	cast_f32_i32
	cast_f32_u32
	cast_f32_i64
	cast_f32_u64
	cast_f32_f64
	cast_f32_d64
	cast_f32_str

	# float64
	f64_nan
	f64_inf
	f64_neg_inf
	f64_pi
	f64_e
	f64_is_nan
	f64_sign
	f64_abs
	f64_sqrt
	f64_exp
	f64_sin
	f64_cos
	f64_tan
	f64_asin
	f64_acos
	f64_atan
	f64_floor
	f64_ceil
	f64_trunc
	f64_round
	f64_ln
	f64_log
	f64_truthy
	f64_neg
	f64_succ
	f64_pred
	f64_add
	f64_sub
	f64_mult
	f64_pow
	f64_div
	f64_idiv
	f64_mod
	f64_mod0
	f64_eq
	f64_ne
	f64_gt
	f64_ge
	f64_lt
	f64_le
	cast_f64_i8
	cast_f64_u8
	cast_f64_i16
	cast_f64_u16
	cast_f64_i32
	cast_f64_u32
	cast_f64_i64
	cast_f64_u64
	cast_f64_f32
	cast_f64_d64
	cast_f64_str

	# dec64
	d64_new
	d64_get_coef
	d64_get_exp
	d64_nan
	d64_pi
	d64_e
	d64_is_nan
	d64_sign
	d64_abs
	d64_sqrt
	d64_exp
	d64_sin
	d64_cos
	d64_tan
	d64_asin
	d64_acos
	d64_atan
	d64_floor
	d64_ceil
	d64_trunc
	d64_round
	d64_ln
	d64_log
	d64_normal
	d64_truthy
	d64_neg
	d64_succ
	d64_pred
	d64_add
	d64_sub
	d64_mult
	d64_pow
	d64_div
	d64_idiv
	d64_mod
	d64_mod0
	d64_eq
	d64_ne
	d64_gt
	d64_ge
	d64_lt
	d64_le
	cast_d64_i8
	cast_d64_u8
	cast_d64_i16
	cast_d64_u16
	cast_d64_i32
	cast_d64_u32
	cast_d64_i64
	cast_d64_u64
	cast_d64_f32
	cast_d64_f64
	cast_d64_str

	# ptr
	ptr_get_deref
	ptr_set_deref
	ptr_get_at
	ptr_set_at
	ptr_add
	ptr_sub
	ptr_addr
	ptr_resized
	ptr_copy_to
	ptr_move_to
	ptr_cmp_with
	ptr_fill_with

	# case id
	caseid_eq
	caseid_ne
	caseid_gt
	caseid_ge
	caseid_lt
	caseid_le

	# debug
	debug_print
>>;

my Opcode %OPCODES{Str};
for Opcode.keys -> $key {
	my $enum = Opcode::{$key};
	if $key eq "O_IF_NOT" {
		%OPCODES<ifnot> = $enum
	} elsif $key eq "O_RET_VOID" {
		%OPCODES<retvoid> = $enum
	} else {
		%OPCODES{$key.subst(/^O_/, "").subst(/_/, "-", :g).lc} = $enum
	}
}

use NativeCall;

# ported from https://github.com/vpisarev/DEC64/blob/alt/dec64_string.c#L356
sub str-to-dec64(Str $str --> int64) {
	my Int @string = $str.ords;

	my Int $at;
	my Int $c;
	my Int $digits;
	my Bool $leading;
	my Bool $ok;
	my Bool $point;

	my int64 $coef;
	my int64 $exp;
	my int64 $exponent;
	my int64 $sign;
	my int64 $sign-exp;

	$c = @string[0];
	$coef = 0;
	$digits = 0;
	$exponent = 0;
	$leading = True;
	$ok = False;
	$point = False;

	if $c == '-'.ord {
		$c = @string[1];
		$at = 1;
		$sign = -1;
	} else {
		$at = 0;
		$sign = 1;
	}

	while defined $c {
		given $c {
			when '0'.ord {
				$ok = True;

				if $leading {
					$exponent -= $point
				} else {
					$digits += 1;
					if $digits > 18 {
						$exponent += 1 - $point
					} else {
						$coef *= 10;
						$exponent -= $point
					}
				}
			}
			when '1'.ord .. '9'.ord {
				$ok = True;
				$leading = False;

				$digits += 1;
				if $digits > 18 {
					$exponent += 1 - $point
				} else {
					$coef = $coef * 10 + ($c - '0'.ord);
					$exponent -= $point
				}
			}
			when '.'.ord {
				if $point {
					die "Parsing error!"
				} else {
					$point = True
				}
			}
			default {
				die "Invalid character"
			}
		}

		$at += 1;
		$c = @string[$at]
	}

	if -128 <= $exponent <= 127 {
		return (($sign * $coef) +< 8) +| ($exponent +& 255)
	} else {
		die "out of range!"
	}
}

#`[
# ported from https://github.com/vpisarev/DEC64/blob/alt/dec64.c#L664
sub dec64-normal(int64 $dec is copy --> int64) {
	my int8 $e = $dec +& 255;
	return $dec if $e == 0;

	$dec = $dec +> 8;
	return 0 if $dec == 0;

	if $e < 0 {
		repeat {
			my int64 $tmp = $dec div 10;
			last if $dec != $tmp*10;
			$dec = $tmp
		} while ++$e < 0;
		return ($dec +< 8) +| $e
	} else {
		$dec = $dec +< 8;

		repeat {
			my int64 $tmp = $dec * 10;
			last if $tmp < $dec; # overflow check
			$dec = $tmp
		} while --$e > 0;
		return $dec +| $e
	}
}
]

class MyBuf does Buf is repr<VMArray> {
	method push-int8(int8 $int) { self.write-int8: self.elems, $int, Endian::LittleEndian }
	method push-uint8(uint8 $int) { self.write-uint8: self.elems, $int, Endian::LittleEndian }

	method push-int16(int16 $int) { self.write-int16: self.elems, $int, Endian::LittleEndian }
	method push-uint16(uint16 $int) { self.write-uint16: self.elems, $int, Endian::LittleEndian }

	method push-int32(int32 $int) { self.write-int32: self.elems, $int, Endian::LittleEndian }
	method push-uint32(uint32 $int) { self.write-uint32: self.elems, $int, Endian::LittleEndian }

	method push-int64(int64 $int) { self.write-int64: self.elems, $int, Endian::LittleEndian }
	method push-uint64(uint64 $int) { self.write-uint64: self.elems, $int, Endian::LittleEndian }

	method push-int128(Int $int) { self.write-int128: self.elems, $int, Endian::LittleEndian }
	method push-uint128(Int $int) { self.write-uint128: self.elems, $int, Endian::LittleEndian }

	method push-num32(num32 $num) { self.write-num32: self.elems, $num, Endian::LittleEndian }

	method push-num64(num64 $num) { self.write-num64: self.elems, $num, Endian::LittleEndian }

	method push-dec64(Str $dec) { self.write-int64: self.elems, str-to-dec64($dec), Endian::LittleEndian }

	method push-type-id(uint32 $id) { self.push-uint32: $id }
	method push-typevar-id(uint16 $id) { self.push-uint16: $id }
	method push-method-id(uint32 $id) { self.push-uint32: $id }
	method push-init-id(uint32 $id) { self.push-uint32: $id }
	method push-member-id(uint16 $id) { self.push-uint16: $id }
	method push-kind-tag(uint16 $id) { self.push-uint16: $id }

	method push-local-id(uint16 $id) { self.push-uint16: $id }
	method push-field-id(uint16 $id) { self.push-uint16: $id }
	method push-loop-id(uint8 $id) { self.push-uint8: $id }

	method push-string(Str $str) {
		self.push-uint32: $str.chars;
		self.append: $str.ords
	}

	method push-bool(Bool $bool) { self.push-uint8: +$bool }

	multi method push-opcode(Opcode $opcode) { self.push-uint8: $opcode.value }
	multi method push-opcode(Str $opcode) { self.push-opcode: %OPCODES{$opcode} }

	method push-nativeop(Str $nativeop) { self.push-uint16: NativeOp::{$nativeop}.value }
}

class StarBCD::Actions {
	has MyBuf $!buf;

	method TOP($/) {
		$!buf .= new("STARVM".ords, 0, 1, 0);

		my %decls{Str};

		for $<decl> -> $decl {
			%decls{$decl<type>.made // $decl<typename>.made // ~$decl<ident>} = $decl<id>.made;
			with $decl<is-main> -> $is-main {
				$!buf.push-type-id: $decl<id>.made;
				$!buf.push-method-id: $is-main<id>.made;
			}
		}

		for $<type> -> $type {
			$!buf.push-type-id: %decls{$type.made}
		}

		$!buf.push-uint32: $<decl>.elems;

		for $<decl> -> $decl {
			$decl.made.()
		}

		make $!buf
	}


	method ident($/) { make ~$/ }
	method label($/) { make ~$/ }
	method typename($/) { make ~$/ }
	method type($/) { make ~$/ }
	method id($/) { make $/.Str.Int }

	method sint($/) { make $/.Str.Int }
	method uint($/) { make $/.Str.UInt }
	method dec($/) { make $/.Str.Num }
	method string($/) {
		make $<contents>.map(-> $m {
			with $m<chars> -> $chars {
				~$chars
			} else {
				"0x$<esc>".Int.chr
			}
		}).join
	}


	method parents($/) {make {
		my @parents = $<typeref>;

		$!buf.push-uint8: +@parents;
		.make.() for @parents
	}}

	method typevars($/) {make {
		my @decls = $<typevar-decl>;

		$!buf.push-uint8: +@decls;
		.made.() for @decls
	}}

	method typevar-decl($/) {make {
		$!buf.push-typevar-id: $<id>.made;
		$!buf.push-string: $<typename>.made;
		with $<parents> { .made.() } else { $!buf.push-uint8: 0 }
	}}
	
	
	method decl:sym<category>($/) {make {
		$!buf.push: 0;
		
		$!buf.push-type-id: $<id>.made;
		$!buf.push-string: $<type>.made;
		with $<typevars> { .made.() } else { $!buf.push-uint8: 0 }

		with $<static-single-methods> { .made.() } else { $!buf.push-uint16: 0 }
		with $<static-multi-methods> { .made.() } else { $!buf.push-uint16: 0 }

		$<path-type>.made.();
		$<for-type>.made.();

		with $<static-members> { .made.() } else { $!buf.push-uint16: 0 }

		with $<static-init> { .made.() } else { $!buf.push-uint32: 0 }

		with $<single-inits> { .made.() } else { $!buf.push-uint16: 0 }
		with $<multi-inits> { .made.() } else { $!buf.push-uint16: 0 }

		with $<inst-single-methods> { .made.() } else { $!buf.push-uint16: 0 }
		with $<inst-multi-methods> { .made.() } else { $!buf.push-uint16: 0 }
		with $<inst-cast-methods> { .made.() } else { $!buf.push-uint16: 0 }
		with $<binary-methods> { .made.() } else { $!buf.push-uint16: 0 }
		with $<unary-methods> { .made.() } else { $!buf.push-uint16: 0 }

		with $<static-deinit> { .made.() } else { $!buf.push-uint32: 0 }
	}}

	method decl:sym<opaquetype>($/) {make {
		$!buf.push: 1;
		
		$!buf.push-type-id: $<id>.made;
		$!buf.push-string: $<type>.made;
		with $<typevars> { .made.() } else { $!buf.push-uint8: 0 }

		with $<static-single-methods> { .made.() } else { $!buf.push-uint16: 0 }
		with $<static-multi-methods> { .made.() } else { $!buf.push-uint16: 0 }
	}}

	method decl:sym<newtype>($/) {make {
		$!buf.push: 2;
		
		$!buf.push-type-id: $<id>.made;
		$!buf.push-string: $<type>.made;
		with $<typevars> { .made.() } else { $!buf.push-uint8: 0 }

		with $<static-single-methods> { .made.() } else { $!buf.push-uint16: 0 }
		with $<static-multi-methods> { .made.() } else { $!buf.push-uint16: 0 }

		$<base>.made.();
		$!buf.push-bool: $<is-noinherit>:exists;
		
		with $<static-members> { .made.() } else { $!buf.push-uint16: 0 }

		with $<static-init> { .made.() } else { $!buf.push-uint32: 0 }

		with $<static-deinit> { .made.() } else { $!buf.push-uint32: 0 }
	}}

	method decl:sym<module>($/) {make {
		$!buf.push: 3;
		
		$!buf.push-type-id: $<id>.made;
		$!buf.push-string: $<type>.made;
		with $<typevars> { .made.() } else { $!buf.push-uint8: 0 }

		with $<static-single-methods> { .made.() } else { $!buf.push-uint16: 0 }
		with $<static-multi-methods> { .made.() } else { $!buf.push-uint16: 0 }

		with $<parents> { .made.() } else { $!buf.push-uint8: 0 }

		with $<static-members> { .made.() } else { $!buf.push-uint16: 0 }

		with $<static-init> { .made.() } else { $!buf.push-uint32: 0 }

		with $<static-deinit> { .made.() } else { $!buf.push-uint32: 0 }
	}}

	method decl:sym<class>($/) {make {
		$!buf.push: 4;
		
		$!buf.push-type-id: $<id>.made;
		$!buf.push-string: $<type>.made;
		with $<typevars> { .made.() } else { $!buf.push-uint8: 0 }

		with $<static-single-methods> { .made.() } else { $!buf.push-uint16: 0 }
		with $<static-multi-methods> { .made.() } else { $!buf.push-uint16: 0 }

		with $<parents> { .made.() } else { $!buf.push-uint8: 0 }

		with $<static-members> { .made.() } else { $!buf.push-uint16: 0 }
		with $<inst-members> { .made.() } else { $!buf.push-uint16: 0 }

		with $<static-init> { .made.() } else { $!buf.push-uint32: 0 }
		with $<default-init> { .made.() } else { $!buf.push-uint32: 0 }

		with $<single-inits> { .made.() } else { $!buf.push-uint16: 0 }
		with $<multi-inits> { .made.() } else { $!buf.push-uint16: 0 }

		with $<inst-single-methods> { .made.() } else { $!buf.push-uint16: 0 }
		with $<inst-multi-methods> { .made.() } else { $!buf.push-uint16: 0 }
		with $<inst-cast-methods> { .made.() } else { $!buf.push-uint16: 0 }
		with $<binary-methods> { .made.() } else { $!buf.push-uint16: 0 }
		with $<unary-methods> { .made.() } else { $!buf.push-uint16: 0 }

		with $<inst-single-method-vtable> { .made.() } else { $!buf.push-uint8: 0 }
		with $<inst-multi-method-vtable> { .made.() } else { $!buf.push-uint8: 0 }
		with $<inst-cast-method-vtable> { .made.() } else { $!buf.push-uint8: 0 }
		with $<binary-method-vtable> { .made.() } else { $!buf.push-uint8: 0 }
		with $<unary-method-vtable> { .made.() } else { $!buf.push-uint8: 0 }

		with $<deinit> { .made.() } else { $!buf.push-uint32: 0 }
		with $<static-deinit> { .made.() } else { $!buf.push-uint32: 0 }
	}}

	method decl:sym<protocol>($/) {make {
		$!buf.push: 5;
		
		$!buf.push-type-id: $<id>.made;
		$!buf.push-string: $<type>.made;
		with $<typevars> { .made.() } else { $!buf.push-uint8: 0 }

		with $<static-single-methods> { .made.() } else { $!buf.push-uint16: 0 }
		with $<static-multi-methods> { .made.() } else { $!buf.push-uint16: 0 }

		with $<parents> { .made.() } else { $!buf.push-uint8: 0 }

		with $<static-members> { .made.() } else { $!buf.push-uint16: 0 }
		with $<inst-members> { .made.() } else { $!buf.push-uint16: 0 }

		with $<static-init> { .made.() } else { $!buf.push-uint32: 0 }
		with $<default-init> { .made.() } else { $!buf.push-uint32: 0 }

		with $<single-inits> { .made.() } else { $!buf.push-uint16: 0 }
		with $<multi-inits> { .made.() } else { $!buf.push-uint16: 0 }

		with $<inst-single-methods> { .made.() } else { $!buf.push-uint16: 0 }
		with $<inst-multi-methods> { .made.() } else { $!buf.push-uint16: 0 }
		with $<inst-cast-methods> { .made.() } else { $!buf.push-uint16: 0 }
		with $<binary-methods> { .made.() } else { $!buf.push-uint16: 0 }
		with $<unary-methods> { .made.() } else { $!buf.push-uint16: 0 }

		with $<inst-single-method-vtable> { .made.() } else { $!buf.push-uint8: 0 }
		with $<inst-multi-method-vtable> { .made.() } else { $!buf.push-uint8: 0 }
		with $<inst-cast-method-vtable> { .made.() } else { $!buf.push-uint8: 0 }
		with $<binary-method-vtable> { .made.() } else { $!buf.push-uint8: 0 }
		with $<unary-method-vtable> { .made.() } else { $!buf.push-uint8: 0 }

		with $<deinit> { .made.() } else { $!buf.push-uint32: 0 }
		with $<static-deinit> { .made.() } else { $!buf.push-uint32: 0 }
	}}

	method decl:sym<tagged-kind>($/) {make {
		$!buf.push: 6;
		
		$!buf.push-type-id: $<id>.made;
		$!buf.push-string: $<type>.made;
		with $<typevars> { .made.() } else { $!buf.push-uint8: 0 }

		with $<static-single-methods> { .made.() } else { $!buf.push-uint16: 0 }
		with $<static-multi-methods> { .made.() } else { $!buf.push-uint16: 0 }

		with $<parents> { .made.() } else { $!buf.push-uint8: 0 }
		$!buf.push-bool: $<is-flags>:exists;

		with $<static-members> { .made.() } else { $!buf.push-uint16: 0 }
		with $<inst-members> { .made.() } else { $!buf.push-uint16: 0 }

		with $<static-init> { .made.() } else { $!buf.push-uint32: 0 }
		with $<default-init> { .made.() } else { $!buf.push-uint32: 0 }

		with $<tagged-cases> { .made.() } else { $!buf.push-uint16: 0 }

		with $<inst-single-methods> { .made.() } else { $!buf.push-uint16: 0 }
		with $<inst-multi-methods> { .made.() } else { $!buf.push-uint16: 0 }
		with $<inst-cast-methods> { .made.() } else { $!buf.push-uint16: 0 }
		with $<binary-methods> { .made.() } else { $!buf.push-uint16: 0 }
		with $<unary-methods> { .made.() } else { $!buf.push-uint16: 0 }

		with $<inst-single-method-vtable> { .made.() } else { $!buf.push-uint8: 0 }
		with $<inst-multi-method-vtable> { .made.() } else { $!buf.push-uint8: 0 }
		with $<inst-cast-method-vtable> { .made.() } else { $!buf.push-uint8: 0 }
		with $<binary-method-vtable> { .made.() } else { $!buf.push-uint8: 0 }
		with $<unary-method-vtable> { .made.() } else { $!buf.push-uint8: 0 }

		with $<deinit> { .made.() } else { $!buf.push-uint32: 0 }
		with $<static-deinit> { .made.() } else { $!buf.push-uint32: 0 }
	}}

	method decl:sym<value-kind>($/) {make {
		$!buf.push: 7;
		
		$!buf.push-type-id: $<id>.made;
		$!buf.push-string: $<type>.made;
		with $<typevars> { .made.() } else { $!buf.push-uint8: 0 }

		with $<static-single-methods> { .made.() } else { $!buf.push-uint16: 0 }
		with $<static-multi-methods> { .made.() } else { $!buf.push-uint16: 0 }

		with $<parents> { .made.() } else { $!buf.push-uint8: 0 }
		$!buf.push-bool: $<is-flags>:exists;
		with $<base> -> $base {
			$!buf.push-bool: True;
			$base.made.();
		} else {
			$!buf.push-bool: False;
		}

		with $<static-members> { .made.() } else { $!buf.push-uint16: 0 }

		with $<static-init> { .made.() } else { $!buf.push-uint32: 0 }

		with $<value-cases> { .made.() } else { $!buf.push-uint16: 0 }

		with $<inst-single-methods> { .made.() } else { $!buf.push-uint16: 0 }
		with $<inst-multi-methods> { .made.() } else { $!buf.push-uint16: 0 }
		with $<inst-cast-methods> { .made.() } else { $!buf.push-uint16: 0 }
		with $<binary-methods> { .made.() } else { $!buf.push-uint16: 0 }
		with $<unary-methods> { .made.() } else { $!buf.push-uint16: 0 }

		with $<inst-single-method-vtable> { .made.() } else { $!buf.push-uint8: 0 }
		with $<inst-multi-method-vtable> { .made.() } else { $!buf.push-uint8: 0 }
		with $<inst-cast-method-vtable> { .made.() } else { $!buf.push-uint8: 0 }
		with $<binary-method-vtable> { .made.() } else { $!buf.push-uint8: 0 }
		with $<unary-method-vtable> { .made.() } else { $!buf.push-uint8: 0 }

		with $<deinit> { .made.() } else { $!buf.push-uint32: 0 }
		with $<static-deinit> { .made.() } else { $!buf.push-uint32: 0 }
	}}


	method single-inits($/) {make {
		$!buf.push-uint16: $<single-init>.elems;
		.made.() for $<single-init>.list;
	}}

	method multi-inits($/) {make {
		$!buf.push-uint16: $<multi-init>.elems;
		.made.() for $<multi-init>.list;
	}}

	method static-single-methods($/) {make {
		$!buf.push-uint16: $<single-method>.elems;
		.made.() for $<single-method>.list;
	}}

	method static-multi-methods($/) {make {
		$!buf.push-uint16: $<multi-method>.elems;
		.made.() for $<multi-method>.list;
	}}

	method inst-single-methods($/) {make {
		$!buf.push-uint16: $<single-method>.elems;
		.made.() for $<single-method>.list;
	}}

	method inst-multi-methods($/) {make {
		$!buf.push-uint16: $<multi-method>.elems;
		.made.() for $<multi-method>.list;
	}}

	method inst-cast-methods($/) {make {
		$!buf.push-uint16: $<cast-method>.elems;
		.made.() for $<cast-method>.list;
	}}

	method binary-methods($/) {make {
		$!buf.push-uint16: $<binary-method>.elems;
		.made.() for $<binary-method>.list;
	}}

	method unary-methods($/) {make {
		$!buf.push-uint16: $<unary-method>.elems;
		.made.() for $<unary-method>.list;
	}}

	method inst-single-method-vtable($/) {make {
		$!buf.push-uint8: $<entries>.elems;
		for $<entries> -> $entry {
			$!buf.push-uint16: $entry<single-method>.elems;
			.made.() for $entry<single-method>.list;
		}
	}}

	method inst-multi-method-vtable($/) {make {
		$!buf.push-uint8: $<entries>.elems;
		for $<entries> -> $entry {
			$!buf.push-uint16: $entry<multi-method>.elems;
			.made.() for $entry<multi-method>.list;
		}
	}}

	method inst-cast-method-vtable($/) {make {
		$!buf.push-uint8: $<entries>.elems;
		for $<entries> -> $entry {
			$!buf.push-uint16: $entry<cast-method>.elems;
			.made.() for $entry<cast-method>.list;
		}
	}}

	method binary-method-vtable($/) {make {
		$!buf.push-uint8: $<entries>.elems;
		for $<entries> -> $entry {
			$!buf.push-uint16: $entry<binary-method>.elems;
			.made.() for $entry<binary-method>.list;
		}
	}}

	method unary-method-vtable($/) {make {
		$!buf.push-uint8: $<entries>.elems;
		for $<entries> -> $entry {
			$!buf.push-uint16: $entry<unary-method>.elems;
			.made.() for $entry<unary-method>.list;
		}
	}}

	method static-init($/)   {make $<opcodes>.made }
	method default-init($/)  {make $<opcodes>.made }
	method deinit($/)        {make $<opcodes>.made }
	method static-deinit($/) {make $<opcodes>.made }

	method static-members($/) {make {
		$!buf.push-uint16: $<member>.elems;
		.made.() for $<member>.list
	}}

	method inst-members($/) {make {
		$!buf.push-uint16: $<member>.elems;
		.made.() for $<member>.list
	}}

	method tagged-cases($/) {make {
		$!buf.push-uint16: $<tagged-case>.elems;
		.made.() for $<tagged-case>.list
	}}

	method value-cases($/) {make {
		$!buf.push-uint16: $<value-case>.elems;
		.made.() for $<value-case>.list
	}}


	method member($/) {make {
		$!buf.push-member-id: $<id>.made;
		$<typeref>.made.();
		#$!buf.push-string: $<ident>.made;
	}}

	method tagged-case($/) {make {
		$!buf.push-kind-tag: $<id>.made;
		with $<slots> -> @slots {
			$!buf.push-string: @slots>><ident>.join;
			$!buf.push-uint8: +@slots;
			for @slots -> (:$typeref, |) {
				$typeref.made.()
			}
		} else {
			$!buf.push-string: $<ident>.made;
			$!buf.push-uint8: 0
		}

		with $<init> { .made.() } else { $!buf.push-uint32: 0 }
	}}

	method value-case($/) {make {
		$!buf.push-kind-tag: $<id>.made;
		$!buf.push-string: $<ident>.made;
		with $<init> { .made.() } else { $!buf.push-uint32: 0 }
	}}


	method single-sig($/) {make {
		$!buf.push-string: $<ident>.made;
	}}
	
	method multi-sig($/) {make {
		my @params = $<params>;
		my $name = "";

		$!buf.push-uint8: +@params;
		for @params -> (:$label, :$typeref, |) {
			$typeref.made.();
			$name ~= $label
		}
		$!buf.push-string: $name
	}}
	
	method single-init($/) {make {
		$!buf.push-init-id: $<id>.made;
		$<opcodes>.made.();
		$<single-sig>.made.()
	}}

	method multi-init($/) {make {
		$!buf.push-init-id: $<id>.made;
		$<opcodes>.made.();
		with $<typevars> { .made.() } else { $!buf.push-uint8: 0 }
		$<multi-sig>.made.()
	}}

	method single-method($/) {make {
		$!buf.push-method-id: $<id>.made;
		$<opcodes>.made.();
		$<single-sig>.made.()
	}}

	method multi-method($/) {make {
		$!buf.push-method-id: $<id>.made;
		$<opcodes>.made.();
		with $<typevars> { .made.() } else { $!buf.push-uint8: 0 }
		$<multi-sig>.made.()
	}}

	method cast-method($/) {make {
		$!buf.push-method-id: $<id>.made;
		$<opcodes>.made.();
		with $<typevars> { .made.() } else { $!buf.push-uint8: 0 }
		$<typeref>.made.()
	}}

	method binary-method($/) {make {
		$!buf.push-method-id: $<id>.made;
		$<opcodes>.made.();
		with $<typevars> { .made.() } else { $!buf.push-uint8: 0 }
		$<param><typeref>.made.();
		$!buf.push-string: ~$<name>
	}}

	method unary-method($/) {make {
		$!buf.push-method-id: $<id>.made;
		$<opcodes>.made.();
		$!buf.push-string: ~$<name>
	}}


	method typeref:sym<decl>($/) {make {
		$!buf.push-uint8: 0;
		$!buf.push-type-id: $<id>.made
	}}

	method typeref:sym<inst>($/) {make {
		$!buf.push-uint8: 1;
		$!buf.push-type-id: $<id>.made;
		$<type-inst-ctx>.made.()
	}}

	method typeref:sym<typevar>($/) {make {
		$!buf.push-uint8: 2;
		$<typevar>.made.()
	}}

	method typeref:sym<this>($/) {make {
		$!buf.push-uint8: 3
	}}

	method typevar-id($/) { make $/.Str.Int }

	method type-inst-ctx($/) {make {
		my @entries = $<entries>;
		
		$!buf.push-uint8: +@entries;
		for @entries -> (:$typevar-id, :$typeref) {
			$!buf.push-typevar-id: $typevar-id.made;
			$typeref.made.()
		}
	}}

	method typevar:sym<dvar>($/) {make {
		$!buf.push-uint8: 0;
		$!buf.push-typevar-id: $<typevar-id>.made
	}}

	method typevar:sym<mvar>($/) {make {
		$!buf.push-uint8: 1;
		$!buf.push-typevar-id: $<typevar-id>.made
	}}


	method typevar-inst-ctx($/) {make {
		my @entries = $<entries>;
		
		$!buf.push-uint8: +@entries;
		for @entries -> (:$typevar, :$typeref, :$mappings?) {
			$typevar.made.();
			$typeref.made.();
			with $mappings {
				$!buf.push-bool: True;
				!!!
			} else {
				$!buf.push-bool: False
			}
		}
	}}


	method local-id($/)  {make { $!buf.push-local-id: $/.Str.Int }}
	method field-id($/)  {make { $!buf.push-field-id: $/.Str.Int }}
	method loop-id($/)   {make { $!buf.push-loop-id: $/.Str.Int }}
	method kind-tag($/)  {make { $!buf.push-kind-tag: $/.Str.Int }}
	method member-id($/) {make { $!buf.push-member-id: $/.Str.Int }}
	method init-id($/)   {make { $!buf.push-init-id: $/.Str.Int }}
	method method-id($/) {make { $!buf.push-method-id: $/.Str.Int }}

	method opcodes($/) {make {
		my @opcodes = $<opcode>;

		$!buf.push-uint32: +@opcodes;
		.made.() for @opcodes
	}}

	method opcode:sym<new-local>($/)        {make { $!buf.push-opcode: ~$<sym> }}
	method opcode:sym<get-local>($/)        {make { $!buf.push-opcode: ~$<sym>; $<local-id>.made.() }}
	method opcode:sym<set-local>($/)        {make { $!buf.push-opcode: ~$<sym>; $<local-id>.made.() }}
	method opcode:sym<tee-local>($/)        {make { $!buf.push-opcode: ~$<sym>; $<local-id>.made.() }}
	method opcode:sym<get-field>($/)        {make { $!buf.push-opcode: ~$<sym>; $<field-id>.made.() }}
	method opcode:sym<set-field>($/)        {make { $!buf.push-opcode: ~$<sym>; $<field-id>.made.() }}
	method opcode:sym<tee-field>($/)        {make { $!buf.push-opcode: ~$<sym>; $<field-id>.made.() }}
	method opcode:sym<get-static-field>($/) {make { $!buf.push-opcode: ~$<sym>; $<field-id>.made.() }}
	method opcode:sym<set-static-field>($/) {make { $!buf.push-opcode: ~$<sym>; $<field-id>.made.() }}
	method opcode:sym<tee-static-field>($/) {make { $!buf.push-opcode: ~$<sym>; $<field-id>.made.() }}

	method opcode:sym<dup>($/)  {make { $!buf.push-opcode: ~$<sym> }}
	method opcode:sym<dup2>($/) {make { $!buf.push-opcode: ~$<sym> }}
	method opcode:sym<swap>($/) {make { $!buf.push-opcode: ~$<sym> }}
	method opcode:sym<pop>($/)  {make { $!buf.push-opcode: ~$<sym> }}

	method opcode:sym<if-else>($/) {make {
		if $<opcodes>.elems == 2 {
			$!buf.push-opcode: O_IF_ELSE;
			$<opcodes>[0].made.();
			$<opcodes>[1].made.();
		} else {
			$!buf.push-opcode: O_IF;
			$<opcodes>[0].made.();
		}
	}}
	method opcode:sym<ifnot>($/) {make { $!buf.push-opcode: ~$<sym>; $<opcodes>.made.() }}
	method opcode:sym<do>($/)    {make { $!buf.push-opcode: ~$<sym>; $<loop-id>.made.(); $<opcodes>.made.() }}
	method opcode:sym<loop>($/)  {make {
		$!buf.push-opcode: ~$<sym>;
		$<loop-id>.made.();
		$<opcodes>[0].made.();
		with $<opcodes>[1] { .made.() } else { $!buf.push-uint32: 0 }
	}}
	method opcode:sym<try>($/)     {make { $!buf.push-opcode: ~$<sym>; $<opcodes>[0].made.(); $<opcodes>[1].made.() }}
	method opcode:sym<ret>($/)     {make { $!buf.push-opcode: ~$<sym> }}
	method opcode:sym<retvoid>($/) {make { $!buf.push-opcode: ~$<sym> }}
	method opcode:sym<throw>($/)   {make { $!buf.push-opcode: ~$<sym>; $!buf.push-string: $<string>.made }}
	method opcode:sym<rethrow>($/) {make { $!buf.push-opcode: ~$<sym> }}
	method opcode:sym<break>($/)   {make { $!buf.push-opcode: ~$<sym>; $<loop-id>.made.() }}
	method opcode:sym<next>($/)    {make { $!buf.push-opcode: ~$<sym>; $<loop-id>.made.() }}

	method opcode:sym<native>($/) {make { $!buf.push-opcode: ~$<sym>; $!buf.push-nativeop: $<ident>.made }}

	method opcode:sym<int8>($/)    {make { $!buf.push-opcode: ~$<sym>; $!buf.push-int8: $<sint>.made }}
	method opcode:sym<uint8>($/)   {make { $!buf.push-opcode: ~$<sym>; $!buf.push-uint8: $<uint>.made }}
	method opcode:sym<int16>($/)   {make { $!buf.push-opcode: ~$<sym>; $!buf.push-int16: $<sint>.made }}
	method opcode:sym<uint16>($/)  {make { $!buf.push-opcode: ~$<sym>; $!buf.push-uint16: $<uint>.made }}
	method opcode:sym<int32>($/)   {make { $!buf.push-opcode: ~$<sym>; $!buf.push-int32: $<sint>.made }}
	method opcode:sym<uint32>($/)  {make { $!buf.push-opcode: ~$<sym>; $!buf.push-uint32: $<uint>.made }}
	method opcode:sym<int64>($/)   {make { $!buf.push-opcode: ~$<sym>; $!buf.push-int64: $<sint>.made }}
	method opcode:sym<uint64>($/)  {make { $!buf.push-opcode: ~$<sym>; $!buf.push-uint64: $<uint>.made }}
	method opcode:sym<float32>($/) {make { $!buf.push-opcode: ~$<sym>; $!buf.push-num32: $<dec>.made }}
	method opcode:sym<float64>($/) {make { $!buf.push-opcode: ~$<sym>; $!buf.push-num64: $<dec>.made }}
	method opcode:sym<dec64>($/)   {make { $!buf.push-opcode: ~$<sym>; $!buf.push-dec64: $<dec>.Str }}
	method opcode:sym<char>($/)    {make { $!buf.push-opcode: ~$<sym>; $!buf.push-uint8: $<uint>.made }}
	method opcode:sym<str>($/)     {make { $!buf.push-opcode: ~$<sym>; $!buf.push-string: $<string>.made }}
	method opcode:sym<true>($/)    {make { $!buf.push-opcode: ~$<sym> }}
	method opcode:sym<false>($/)   {make { $!buf.push-opcode: ~$<sym> }}
	method opcode:sym<this>($/)    {make { $!buf.push-opcode: ~$<sym> }}

	method opcode:sym<block>($/) {make { $!buf.push-opcode: ~$<sym>; $<opcodes>.made.() }}

	method opcode:sym<vcase-id>($/)      {make { $!buf.push-opcode: ~$<sym>; $<typeref>.made.(); $<kind-tag>.made.() }}
	method opcode:sym<tcase-id>($/)      {make { $!buf.push-opcode: ~$<sym>; $<typeref>.made.(); $<kind-tag>.made.() }}
	method opcode:sym<kind-id>($/)       {make { $!buf.push-opcode: ~$<sym> }}
	method opcode:sym<kind-slot>($/)     {make { $!buf.push-opcode: ~$<sym>; $!buf.push-uint8: $<slot>.made }}
	method opcode:sym<kind-value>($/)    {make { $!buf.push-opcode: ~$<sym> }}
	method opcode:sym<upcast>($/)        {make { $!buf.push-opcode: ~$<sym>; $<typeref>.made.() }}
	method opcode:sym<downcast>($/)      {make { $!buf.push-opcode: ~$<sym>; $<typeref>.made.() }}
	method opcode:sym<native-cast>($/)   {make { $!buf.push-opcode: ~$<sym>; $<typeref>.made.() }}
	method opcode:sym<dynamic-cast>($/)  {make { $!buf.push-opcode: ~$<sym>; $<typeref>.made.() }}
	method opcode:sym<of-type>($/)       {make { $!buf.push-opcode: ~$<sym>; $<typeref>.made.() }}
	method opcode:sym<new-ptr>($/)       {make { $!buf.push-opcode: ~$<sym>; $<typeref>.made.() }}
	method opcode:sym<ptr-from-addr>($/) {make { $!buf.push-opcode: ~$<sym>; $<typeref>.made.() }}

	method opcode:sym<get-member>($/)        {make { $!buf.push-opcode: ~$<sym>; $<member-id>.made.() }}
	method opcode:sym<set-member>($/)        {make { $!buf.push-opcode: ~$<sym>; $<member-id>.made.() }}
	method opcode:sym<tee-member>($/)        {make { $!buf.push-opcode: ~$<sym>; $<member-id>.made.() }}
	method opcode:sym<get-static-member>($/) {make { $!buf.push-opcode: ~$<sym>; $<typeref>.made.(); $<member-id>.made.() }}
	method opcode:sym<set-static-member>($/) {make { $!buf.push-opcode: ~$<sym>; $<typeref>.made.(); $<member-id>.made.() }}
	method opcode:sym<tee-static-member>($/) {make { $!buf.push-opcode: ~$<sym>; $<typeref>.made.(); $<member-id>.made.() }}

	method opcode:sym<default-init>($/) {make { $!buf.push-opcode: ~$<sym>; $<typeref>.made.() }}
	method opcode:sym<init-this-s>($/)  {make { $!buf.push-opcode: ~$<sym>; $<typeref>.made.(); $<init-id>.made.() }}
	method opcode:sym<init-this-m>($/)  {make { $!buf.push-opcode: ~$<sym>; $<typeref>.made.(); $<init-id>.made.(); with $<typevar-inst-ctx> { .made.() } else { $!buf.push-uint8: 0 }} }
	method opcode:sym<send-is>($/)      {make { $!buf.push-opcode: ~$<sym>; $<typeref>.made.(); $<init-id>.made.() }}
	method opcode:sym<send-im>($/)      {make { $!buf.push-opcode: ~$<sym>; $<typeref>.made.(); $<init-id>.made.(); with $<typevar-inst-ctx> { .made.() } else { $!buf.push-uint8: 0 }} }
	method opcode:sym<send-ss>($/)      {make { $!buf.push-opcode: ~$<sym>; $<typeref>.made.(); $<method-id>.made.() }}
	method opcode:sym<send-ms>($/)      {make { $!buf.push-opcode: ~$<sym>; $<typeref>.made.(); $<method-id>.made.(); with $<typevar-inst-ctx> { .made.() } else { $!buf.push-uint8: 0 }} }
	method opcode:sym<send-si>($/)      {make { $!buf.push-opcode: ~$<sym>; $<typeref>.made.(); $<method-id>.made.() }}
	method opcode:sym<send-dyn-si>($/)  {make { $!buf.push-opcode: ~$<sym>; $<typeref>.made.(); $<method-id>.made.() }}
	method opcode:sym<send-mi>($/)      {make { $!buf.push-opcode: ~$<sym>; $<typeref>.made.(); $<method-id>.made.(); with $<typevar-inst-ctx> { .made.() } else { $!buf.push-uint8: 0 }} }
	method opcode:sym<send-dyn-mi>($/)  {make { $!buf.push-opcode: ~$<sym>; $<typeref>.made.(); $<method-id>.made.(); with $<typevar-inst-ctx> { .made.() } else { $!buf.push-uint8: 0 }} }
	method opcode:sym<send-c>($/)       {make { $!buf.push-opcode: ~$<sym>; $<typeref>.made.(); $<method-id>.made.(); with $<typevar-inst-ctx> { .made.() } else { $!buf.push-uint8: 0 }} }
	method opcode:sym<send-dyn-c>($/)   {make { $!buf.push-opcode: ~$<sym>; $<typeref>.made.(); $<method-id>.made.(); with $<typevar-inst-ctx> { .made.() } else { $!buf.push-uint8: 0 }} }
	method opcode:sym<send-bo>($/)      {make { $!buf.push-opcode: ~$<sym>; $<typeref>.made.(); $<method-id>.made.(); with $<typevar-inst-ctx> { .made.() } else { $!buf.push-uint8: 0 }} }
	method opcode:sym<send-dyn-bo>($/)  {make { $!buf.push-opcode: ~$<sym>; $<typeref>.made.(); $<method-id>.made.(); with $<typevar-inst-ctx> { .made.() } else { $!buf.push-uint8: 0 }} }
	method opcode:sym<send-uo>($/)      {make { $!buf.push-opcode: ~$<sym>; $<typeref>.made.(); $<method-id>.made.() }}
	method opcode:sym<send-dyn-uo>($/)  {make { $!buf.push-opcode: ~$<sym>; $<typeref>.made.(); $<method-id>.made.() }}

	method opcode:sym<init-class>($/)       {make { $!buf.push-opcode: ~$<sym>; $<typeref>.made.() }}
	method opcode:sym<init-tkind>($/)       {make { $!buf.push-opcode: ~$<sym>; $<typeref>.made.(); $<kind-tag>.made.() }}
	method opcode:sym<init-vkind>($/)       {make { $!buf.push-opcode: ~$<sym>; $<typeref>.made.(); $<kind-tag>.made.() }}
	method opcode:sym<init-multi-tkind>($/) {make { $!buf.push-opcode: ~$<sym>; $<typeref>.made.(); $<kind-tag>.made.() }}
	method opcode:sym<init-multi-vkind>($/) {make { $!buf.push-opcode: ~$<sym>; $<typeref>.made.(); $<kind-tag>.made.() }}
	
	method opcode:sym<multi-kind-has-tag>($/)  {make { $!buf.push-opcode: ~$<sym>; $<kind-tag>.made.() }}
	method opcode:sym<multi-kind-get-tag>($/)  {make { $!buf.push-opcode: ~$<sym>; $<kind-tag>.made.() }}
	method opcode:sym<multi-kind-get-slot>($/) {make { $!buf.push-opcode: ~$<sym>; $<kind-tag>.made.(); $!buf.push-uint8: $<slot-id>.made }}
}

my $buf = StarBCD::Grammar.parse(slurp("input.starbcd"), :actions(StarBCD::Actions.new)).made;
"output.starbc".IO.spurt: $buf