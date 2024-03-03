import dotenv
from langchain.text_splitter import Language
from langchain_community.document_loaders.generic import GenericLoader
from langchain_community.document_loaders.parsers import LanguageParser
from langchain.text_splitter import RecursiveCharacterTextSplitter
from langchain_community.vectorstores import Chroma
from langchain.chains import ConversationalRetrievalChain
from langchain.memory import ConversationSummaryMemory
from langchain_openai import ChatOpenAI
from langchain_community.embeddings import LlamaCppEmbeddings
from langchain_core.runnables import RunnableParallel, RunnablePassthrough
from langchain_core.output_parsers import StrOutputParser
from langchain_core.prompts import ChatPromptTemplate



dotenv.load_dotenv()

repo_path = "/home/remote/Projects/opensource/ollama"

loader = GenericLoader.from_filesystem(
        repo_path + "/",
        glob="**/*",
        suffixes=[".go"],
        exclude=["**/non-utf8-encoding.py"],
        parser=LanguageParser(language=Language.PYTHON, parser_threshold=500),
        )
documents = loader.load()
len(documents)

python_splitter = RecursiveCharacterTextSplitter.from_language(
        language=Language.GO, chunk_size=2000, chunk_overlap=200)
texts = python_splitter.split_documents(documents)
len(texts)

embedding = LlamaCppEmbeddings(model_path="nomic-embed-text-v1.5.Q8_0.gguf", verbose=False)

db = Chroma.from_documents(texts, embedding)
retriever = db.as_retriever(
        search_type="mmr",  # Also test "similarity"
        search_kwargs={"k": 8})


llm = ChatOpenAI(model_name="mistral-7b-instruct-fp16")
memory = ConversationSummaryMemory(
            llm=llm, memory_key="chat_history", return_messages=True
            )
qa = ConversationalRetrievalChain.from_llm(llm, retriever=retriever, memory=memory)

template = """You are a coding compleition engine. Your job is to help a developer code faster
for an experimentation project on which he is working locally. When provided with some sample
codes and the parital code, you provide a well commented code that should complete
the partial code. Use the same language as the sample codes. Do not use a different
language. Do not output code in Python if not explicitly requested to do so. Output
the code between tripple backticks(```). Don't output anything that should occur before the
given code.

Following are the sample codes:
{context}

Complete the following code:
```
{partial_code}
```
"""
prompt = ChatPromptTemplate.from_template(template)


class CodeOutputParser(StrOutputParser):
    def parse(self, text: str) -> str:
        print(text)
        """Returns the code from the input."""
        parts = text.split("```")
        if len(parts) > 1:
            return parts[1]
        return ""


output_parser = CodeOutputParser()


def format_docs(docs):
    # print([x for x in docs if 'language' not in x.metadata])
    ret = ""
    for d in docs:
        if "language" in d.metadata:
            ret += f'language: {d.metadata["language"]}\n'
        ret += f'file name: {d.metadata["source"]}\ncontents:\n{d.page_content}\n'
        ret += "\n=======================================\n" 


setup_and_retrieval = RunnableParallel(
    {"context": retriever | format_docs, "partial_code": RunnablePassthrough()}
)

chain = setup_and_retrieval | prompt | llm | output_parser

print("="*20)

partial_code = """
    convertCmd := &cobra.Command{
        Use:     "convert MODEL",
        Short:   "Convert the Tensor model file into gguf format",
"""

result = chain.invoke(partial_code)
print(result)

