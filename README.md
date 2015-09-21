Small Tool to handle register-emails

Rechnung erzeugen:
<pre>./LBASregister rechnung "../lbas15orga/templates/rechnung-template.tex" 
</pre>

Emailadresse:
<pre>./LBASregister email </pre>

CSVZeile 
<pre>./LBASregister csv</pre>


## Input

A directory containing YAML-files with the fields given in RegistrationParser.hs (Registry). For example:

<pre>
Anrede: 
Vorname: 
Nachname: 
Titel: 
Affiliation: 
Street: 
City: 
Postcode: 
Email: 
</pre>

This is not perfect and I am open to suggestions. 