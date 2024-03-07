# Lem Codecomp


## Usage

- Clone the repo
```
$ git clone https://github.com/asutoshpalai/lem-codecomp
$  cd lem-codecomp
```

- Download the embedding model
```
$ curl -OL 'https://huggingface.co/nomic-ai/nomic-embed-text-v1.5-GGUF/resolve/main/nomic-embed-text-v1.5.Q8_0.gguf?download=true'
```

- Export the following environment variables or create an `.env` file contianing
the following variables.

```
export OPENAI_BASE_URL="https://api.openai.com/v1"
export OPENAI_API_KEY="skynet-launch-codes"
export OPENAI_MODEL_NAME="gpt-3.5-turbo-0613"
```

- Launch the Python server from the same shell. I haven't commited the dependency
file yet. Please install anything that it complains about. Some errors might show
up while serving the request.

```
$ python server.py
```

- Test that the server is working properly.
```
curl -X POST -v http://localhost:8000/code_complete/invoke --header 'Content-Type: application/json' --data-raw '{
        "input": {
        "repo_path": "/home/user/projects/lem-codecomp/",
        "language": "python",
        "partial_code": "def start_server(host: str, port: int):\n"
  }
}'
```

- Load this in your Lem's `init.lisp` file. Add the following lines to the init file.
```
(push "/home/remote/Learning/llm/anyscale/" asdf:*central-registry*)
(ql:quickload :lem-codecomp)
```