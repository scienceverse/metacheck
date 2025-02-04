from langchain.chains import ConversationalRetrievalChain, RetrievalQAWithSourcesChain
from langchain_openai import ChatOpenAI
from langchain_community.document_loaders import TextLoader
from langchain_openai import OpenAIEmbeddings
from langchain.text_splitter import RecursiveCharacterTextSplitter
from langchain_community.vectorstores import FAISS
from langchain_core.prompts import ChatPromptTemplate, PromptTemplate
from langchain_community.callbacks import get_openai_callback

# https://stackoverflow.com/questions/53014306/error-15-initializing-libiomp5-dylib-but-found-libiomp5-dylib-already-initial
import os
os.environ['KMP_DUPLICATE_LIB_OK']='True'


def py_gpt(file, query, context, APIKEY, gpt_model = "gpt-3.5-turbo-1106", chunk_size = 500, chunk_overlap = 100, temperature = 0):
  # Loads personal texts for model
  loader = TextLoader(file)
  documents = loader.load()
  
  # Text splitters are needed to split long texts into chunks
  # Must experiment to figure out the best chunk_size and chunk_overlap!
  # (params of RecursiveCharacterTextSplitter)
  text_splitter = RecursiveCharacterTextSplitter(chunk_size=chunk_size, chunk_overlap=chunk_overlap)
  texts = text_splitter.split_documents(documents)

  # Creating vectorstore index is needed for model to efficiently look through your provided data
  # Essentially it is changing the text into a vector that becomes a series of numbers that hold the semantic 'meaning' of the text
  # There are many different vector stores, now we use Facebook AI Similarity Search (FAISS)
  embeddings = OpenAIEmbeddings(openai_api_key=APIKEY)
  
  vector_db = FAISS.from_documents(texts, embeddings)

  retriever = vector_db.as_retriever()
  
  # Find out more about the models behavior in relation to the query:
  docs = retriever.invoke(query)
  # print('Most relevant docs to your query:')
  # print("\n\n".join([x.page_content[:200] for x in docs[:5]]))
  
  # Read a bit about embeddings in lanchain docs:
  # https://python.langchain.com/v0.1/docs/modules/data_connection/text_embedding/
  text_embedding = embeddings.embed_query(query)
  #print (f"The complete embedding length is {len(text_embedding)}")
  
  # The template can be used to give instructions on how to answer
  # (A lot of variety possible here, learn a bit from;
  # https://github.com/gkamradt/langchain-tutorials/blob/main/LangChain%20Cookbook%20Part%201%20-%20Fundamentals.ipynb )
  # Leave in {summaries} and {question} for these input variables must be in the prompt template the chain that will be built after
  template = """
  {context}
  {summaries}
  {question}
  """


  # Yet to determine optimal chain_type
  # (https://docs.langflow.org/components/chains#:~:text=chain_type%3A%20The%20chain%20type%20to,that%20prompt%20to%20an%20LLM.)
  chain = RetrievalQAWithSourcesChain.from_chain_type(
      llm=ChatOpenAI(openai_api_key=APIKEY, model=gpt_model, temperature=temperature),
      chain_type="stuff",
      retriever=retriever,
      chain_type_kwargs={
          "prompt": PromptTemplate(
              template=template,
              input_variables=["summaries", "question"],
          ),
      },
  )
  
  # For the chain we define a chat history that we will fill with every answer and loop back into new prompts, this way we can converse with the model
  chat_history = []
  # Openai Callback is not necessary but nice if you want to print statistics about the prompt later.
  with get_openai_callback() as cb:
      result = chain.invoke(input={"question": query, "context": context, "chat_history": chat_history})
  
  return {"result": result, "callback": cb, "docs": docs}
