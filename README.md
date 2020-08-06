# A Tiny HTTP Server for Testing HTTP Clients

[![CI](https://github.com/pjones/mock-httpd/workflows/CI/badge.svg)](https://github.com/pjones/mock-httpd/actions)
[![GitHub tag (latest by date)](https://img.shields.io/github/v/tag/pjones/mock-httpd?label=release)](https://github.com/pjones/mock-httpd/releases)
[![Hackage](https://img.shields.io/hackage/v/mock-httpd)](https://hackage.haskell.org/package/mock-httpd)

Given a configuration file describing routes and actions, `mock-httpd`
will bind to the specified ports and respond to incoming HTTP
requests.

## Configuration

A YAML configuration file is used to control `mock-httpd`.  The file
is split between general configuration and routing information.

### General Configuration

The following settings control all requests:

  * `listen`: An array of directives, each of which causes
    `mock-httpd` to bind to a port and listen for requests.

    Example:

    ```yml
    listen:
      - port: 3210
    ```

  * `directory`: A path to a directory where files can be read from and
    written to.  All routing actions involving files are restricted to
    this directory.

An [example configuration](example.yml) is included with the distribution.

### Routing Information

The `routing` section of the configuration file is used to match
incoming requests and perform corresponding actions.

Example:

```yml
routes:
  - match_path: /hello
    match_method: GET
    actions:
      - serve_text: "Hello World!\n"
      - content_type: text/plain
```

#### Route Matching

Match directives (properties starting with `match_`) are used to
select actions to perform for incoming requests.  All listed match
directives must match the incoming request in order for the containing
route's actions to be performed.  Only the first matching route is used.

  * `match_path`: Exact string comparison with the requested path.

  * `match_method`: Exact string comparison with the request method.

#### Actions

The following actions can be taken when a route matches:

  * `serve_file: path`: Send the specified file back to the client.

  * `serve_text: text`: Send arbitrary text back to the client.

  * `location: url`: Set the `Location` header.

  * `content_type: type`: Set the `Content-Type` header.

  * `set_headers: hash`: Set each entry in the specified hash as a
    response header.

  * `response_code: code`: Set the response code to `code`.

  * `save_body: path`: Write the request body to the specified file.

**NOTE:** The `serve_*` actions are mutually exclusive and only the
first one will be used.  If no `serve_*` action is specified the
default action is to respond with an empty body.
