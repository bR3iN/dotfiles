$0 ~ "^\[.*\]" { 
		gsub(/(\[( |	)*|( |	)*\])/,"")
		print
	}
