package main

import (
	"net/smtp"
)

func main() {
	auth := smtp.PlainAuth(
		"",
		"mark@heroku.com",
		"xxx",
		"smtp.gmail.com",
	)

	err := smtp.SendMail(
		"smtp.gmail.com:25",
		auth,
		"mark+sent@heroku.com",
		[]string{"mark@heroku.com"},
		[]byte("The Subject\r\n\r\nThe Body"),
	)
	if err != nil { panic(err) }
}

// missing subject, cc, bcc, attachements, plain/multi-type emails
// https://github.com/mtoader/google-go-lang-idea-plugin/issues/112
