# Turtle Parsing

## Numbers abbreviation

The numbers abbreviation
and string representation
(matching) the same type
are treated as exactly the same.

## Base implementation

Base is always added at the start.

Example:
```ttl
@base <http://example.org/> .
<thing> <is> <http://url.org/thing> .
```

Generates the following triples:
```
<http://example.org/thing> <http://example.org/is> <http://example.org/http://url.org/thing> .
```

Some people may expect that
the object is `<http://url.org/thing>`
instead of the actual result
`<http://example.org/http://url.org/thing>`.

## IRI normalization

We do not do IRI normalization.

Example:
`http://example.org/page#asdf`
is treated differently to
`http://example.org/page/#asdf`
