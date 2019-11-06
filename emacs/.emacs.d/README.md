If getting error `Failed to verify signature`, do 

- `M-x`  
- `set-variable` 
- `RET`
- `package-check-signature`
- `RET`
- `nil`
- `RET`
- `M-x`
- `package-install`
- `RET`
- `gnu-elpa-keyring-update`

Restart emacs
