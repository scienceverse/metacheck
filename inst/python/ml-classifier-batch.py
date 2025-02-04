from transformers import BertTokenizer, BertForSequenceClassification
import torch
from torch.utils.data import Dataset, DataLoader

def py_ml_classifier(texts, model_dir):
    #model_dir = './sample-size/'
    model = BertForSequenceClassification.from_pretrained(model_dir)
    tokenizer = BertTokenizer.from_pretrained(model_dir)

    # Tokenize input text
    #tokenized_input = tokenizer(text, padding=True, truncation=True, return_tensors="pt", max_length=512)

    # Step 2: Tokenize the text data
    class TextDataset(Dataset):
        def __init__(self, texts, tokenizer, max_length=512):
            self.texts = texts
            self.tokenizer = tokenizer
            self.max_length = max_length
        
        def __len__(self):
            return len(self.texts)
        
        def __getitem__(self, idx):
            text = self.texts[idx]
            encoding = self.tokenizer(
                text,
                max_length=self.max_length,
                padding='max_length',
                truncation=True,
                return_tensors='pt'
            )
            return encoding
    
    # Step 3: Create a Dataset and DataLoader
    dataset = TextDataset(texts, tokenizer)
    dataloader = DataLoader(dataset, batch_size=8, shuffle=False)

    # Ensure the model is in evaluation mode
    model.eval()
    predictions = []
    probabilities = []

    # Make predictions
    with torch.no_grad():
        # Forward pass
        #outputs = model(**tokenized_input)
        for batch in dataloader:
            input_ids = batch['input_ids'].squeeze(1)
            attention_mask = batch['attention_mask'].squeeze(1)
            outputs = model(input_ids=input_ids, attention_mask=attention_mask)
            probs = torch.nn.functional.softmax(outputs.logits, dim=-1)
            preds = torch.argmax(probs, dim=1)
            #preds = torch.argmax(outputs.logits, dim=1)
            predictions.extend(preds.numpy())
            probabilities.extend(probs.numpy())
            

    # Get the predicted probabilities
    #probs = torch.nn.functional.softmax(outputs.logits, dim=-1)

    # Get the predicted class (0 or 1 in binary classification, 0 == No N-Value, 1 == N-Value)
    #predicted_class = torch.argmax(probs, dim=1).tolist()

    # Display results
    #return { "classification": predicted_class[0], "probs": probs }
    return { "classification": predictions, "probs": probabilities }
