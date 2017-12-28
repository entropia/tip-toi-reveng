/* 
 * ***************************************** *
 * regex of the Yamlcode for the tttool  * *
 * ***************************************** *  
 */
 
Prism.languages.yamltttool = {

	'comment': /#.*/,
	'function': {
		pattern: /(\s*(?:^|[:\-,[{\r\n?])[ \t]*(![^\s]+)?[ \t]*)[^\r\n{[\]},#\s]+?(?=\s*:\s)/,
		lookbehind: true
	},
	'directive': {
		/* pattern: /(T)\((([a-zA-Z0-9]+)[a-zA-Z0-9_]*,)*([a-zA-Z0-9]+)[a-zA-Z0-9_]*\)/ */
		pattern: /(T)\(.*\)/,
			'inside': {
			'variable': /(\$)([a-zA-Z0-9_])+/,
			'number': /(\d)+/
		}

	},

	'boolean': 
	{
	pattern: /J\([a-zA-Z0-9]+[a-zA-Z0-9_]*\)/
	},
	
	'char': [
    {
	pattern: /(PA|PA\*)\((([a-zA-Z0-9]+)[a-zA-Z0-9_]*(,|-)\s?)*([a-zA-Z0-9]+)[a-zA-Z0-9_]*\)/ 
	}
	],
	

	'string':  {
		pattern: /(P|P\*)\(([a-zA-Z0-9]+[a-zA-Z0-9_]*,\s?)*[a-zA-Z0-9_]*\)/
	 },
    'symbol': {
		pattern: /(\?)\(.*\)\s\(.*\)/,
				inside: {
			'variable': /(\$)([a-zA-Z0-9_])+/,
			'attr-value': /[0-9a-fA-F]{2}\s[0-9a-fA-F]{2}/,
			'number': /(\d)+/
		}

	},
	'selector':  {
		pattern: /G\(([a-zA-Z0-9]+[a-zA-Z0-9_]*,\s?)*[a-zA-Z0-9_]*\)/
	 },
	'entity':  {
		pattern: /C\(([a-zA-Z0-9]+[a-zA-Z0-9_]*,\s?)*[a-zA-Z0-9_]*\)/
	 },
	
	'variable': [
		{
			pattern: /(\$)([a-zA-Z0-9_])+/
		},
		{
			pattern: /(\$)(\d)*/
		}
	],

	'number': {
		pattern: /(\d)+/
	}
};

