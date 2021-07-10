all: compile

run:
	./run

bounce:
	sudo svc -du /etc/service/$$(basename $$(pwd))

start-service:
	sudo svc -u /etc/service/$$(basename $$(pwd))

stop-service:
	sudo svc -d /etc/service/$$(basename $$(pwd))

compile:
	raco make src/main.rkt src/site.rkt

clean:
	find . -depth -type d -iname compiled -exec rm -rf {} \;

.PHONY: run bounce

###########################################################################

keys: private-key.pem server-cert.pem

private-key.pem:
	openssl genrsa -des3 -passout pass:dummy -out $@ 4096
	openssl rsa -passin pass:dummy -in $@ -out $@

server-cert.pem: private-key.pem
	openssl req -new -x509 -nodes -sha1 -days 365 \
		-subj /CN=beta.package.database.site \
		-passin pass:dummy \
		-key private-key.pem > $@

clean-keys:
	rm -f private-key.pem server-cert.pem
