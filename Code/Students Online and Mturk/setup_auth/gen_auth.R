#setup the web-auth for interactive tax online
setwd("~/Dropbox/Ray_Projects/shared_folders/CESS_Aki/Interactive Online Tax Experiment/setup_auth/")
set.seed('20160301')
auth.codes <- rep(NA, 1000)
exit.codes <- rep(NA, 1000)

for(i in 1:length(auth.codes)){
auth.codes[i] <- sprintf("AU%s", paste(sample(c(LETTERS, 0:9), 18), collapse = ""))
exit.codes[i] <- sprintf("E%s", paste(sample(c(LETTERS, letters, 0:9), 15), collapse = ""))
}

scr <- sprintf("            { id: '%s', ExitCode: '%s'},", auth.codes, exit.codes)
write(scr, file = "auth_codes.txt")


sql <- sprintf("insert into interactive_tax_auth (auth_id) values (\"%s\");", auth.codes)
write(sql, file = 'sql.txt')
