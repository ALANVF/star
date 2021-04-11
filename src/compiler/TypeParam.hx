package compiler;

@:build(util.Auto.build())
class TypeParam {
	var type: Option<Type> = None;
	var name: Option<String> = None;
	var params: Option<Array<TypeParam>> = None;
	var value: Option<Type> = None;
	var isVariadic: Bool = false;
}