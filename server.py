from langchain.text_splitter import Language
from langchain_community.document_loaders.generic import GenericLoader
from langchain_community.document_loaders.parsers import LanguageParser
from langchain_community.embeddings import LlamaCppEmbeddings
from langchain.text_splitter import RecursiveCharacterTextSplitter
from langchain_community.vectorstores import Chroma
from langchain_core.runnables import chain
from langchain_core.output_parsers import StrOutputParser
from fastapi import FastAPI
from langchain.prompts import ChatPromptTemplate
from langchain_core.prompt_values import PromptValue
from langchain_openai import ChatOpenAI
from langserve import add_routes
import dotenv
from operator import itemgetter
from typing import List, Dict
from dataclasses import dataclass
import logging
import os


dotenv.load_dotenv()
logger = logging.getLogger(__name__)

# Set the logging level to DEBUG
logger.setLevel(logging.DEBUG)

# Create a StreamHandler and add it to the logger
handler = logging.StreamHandler()
logger.addHandler(handler)


def getFileSuffixes(lang: Language) -> List[str]:
    if lang == Language.GO:
        return [".go"]
    if lang == Language.PYTHON:
        return [".py"]
    raise Exception("unknown language")


class CodeOutputParser(StrOutputParser):
    def parse(self, text: str) -> str:
        logger.debug(text)
        """Returns the code from the input."""
        parts = text.split("```")
        if len(parts) > 1:
            return parts[1]
        return ""


retriver_cache = {}
embedding = LlamaCppEmbeddings(model_path="nomic-embed-text-v1.5.Q8_0.gguf", verbose=False)


@dataclass
class RetInput:
    repo_path: str
    language: Language
    partial_code: str


@chain
def retriever_wrap(input: RetInput) -> Dict[str, str]:
    docs = retriever(input.repo_path, input.language, input.partial_code)
    ctx = format_docs(docs)

    return {"partial_code": input.partial_code, "context": ctx}


def retriever(repo_path: str, lang: Language, partial_code: str) -> List[str]:
    # TODO: add persistance to MAYBE reduce memory footprint?
    if not os.path.isdir(repo_path):
        logger.debug("received file path, converting it to dir path")
        repo_path = os.path.dirname(repo_path)

    id = hex(hash(repo_path + lang))[2:]  # not designed to be secure, but to be unique

    if id in retriver_cache:
        logger.debug("using cached retriver")
        # return retriver_cache[repo_path].invoke(partial_code)
        return retriver_cache[id].invoke(partial_code)

    logger.debug(f"creating retiever for {repo_path} and lang {lang}")
    loader = GenericLoader.from_filesystem(
        repo_path + "/",
        glob="**/*",
        suffixes=getFileSuffixes(lang),
        parser=LanguageParser(language=lang, parser_threshold=500),
        )
    documents = loader.load()
    splitter = RecursiveCharacterTextSplitter.from_language(
        language=lang, chunk_size=2000, chunk_overlap=200)
    texts = splitter.split_documents(documents)
    db = Chroma.from_documents(texts, embedding, collection_name=id)
    ret = db.as_retriever(
        search_type="mmr",  # Also test "similarity"
        search_kwargs={"k": 8})

    retriver_cache[id] = ret
    return ret.invoke(partial_code)


def format_docs(docs):
    # print([x for x in docs if 'language' not in x.metadata])
    ret = ""
    for d in docs:
        if "language" in d.metadata:
            ret += f'language: {d.metadata["language"]}\n'
        ret += f'file name: {d.metadata["source"]}\ncontents:\n{d.page_content}\n'
        ret += "\n=======================================\n"
    return ret


llm = ChatOpenAI(model_name=os.getenv("OPENAI_MODEL_NAME"))

template = """You are a coding compleition engine. Your job is to help a developer code faster
for an experimentation project on which he is working locally. When provided with some sample
codes and the parital code, you provide a well commented code that should complete
the partial code. Here are the instructions that you need to follow:
1. Use the same language as the sample codes. Do not use a different language.
2. Do not output code in Python if not explicitly requested to do so.
3. Output the code between tripple backticks(```).
4. Do not output anything that should occur before the given code.
5. The exact last line of the parital code should be present in the output.

Following are the sample codes:
{context}

Complete the following code:
```
{partial_code}
```
"""

prompt = ChatPromptTemplate.from_template(template)


@chain
def printPrompt(inp: PromptValue) -> PromptValue:
    logger.debug("-" * 10 + "\nprompt: " + str(inp) + "-" * 10)
    return inp


chain = retriever_wrap | prompt | printPrompt | llm | CodeOutputParser()

partial_code = """
    convertCmd := &cobra.Command{
        Use:     "convert MODEL",
        Short:   "Convert the Tensor model file into gguf format",
"""
# result = chain.invoke({
#    "repo_path": "/home/remote/Projects/opensource/ollama",
#    "language": "go",
#    "partial_code": partial_code})

# print(result)

app = FastAPI(
    title="LangChain Server",
    version="1.0",
    description="A simple api server using Langchain's Runnable interfaces",
)

add_routes(
    app,
    chain,
    path="/code_complete",
)

if __name__ == "__main__":
    import uvicorn
    # logging.basicConfig(level=logging.DEBUG)
    logger.debug("hhhhhhhhhhhhhh")

    uvicorn.run(app, host="0.0.0.0", port=8000)
