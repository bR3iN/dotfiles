function todo
    if test -f /shared/todo.txt
        nvim /shared/todo.txt
    else if test -f /var/shared/todo.txt
        nvim /var/shared/todo.txt
    end
end
