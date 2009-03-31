prob.html: Prob.lhs header.html
	pandoc -o prob.html -f mkd Prob.lhs -s

clean:
	rm -f prob.html
	runhaskell Setup.hs clean