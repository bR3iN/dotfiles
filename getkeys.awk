BEGIN {
in_target_section = 0 
output_seperator = ":"
s = "( |	)*"
}

# Matches non-target header
$0 ~ "^\[.*\]" \
	&& $0 !~ "^\["s TARGET_SECTION s"\]" \
	{
		in_target_section = 0 
	}

in_target_section \
	&& $0 !~ "^"s"\;" \
	&& $0 ~ s"[a-zA-Z0-9.]" \
	{
		gsub(/(^(	| )+|(	| )+$)/,"",$1)
		gsub(/(^(	| )+|(	| )+$)/,"",$2)
		print $1 output_seperator $2
	}
	
# Matches target header
$0 ~ "^\["s TARGET_SECTION s"\]" \
	{
		in_target_section = 1 
	}

