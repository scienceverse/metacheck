from transformers import BertTokenizer, BertForSequenceClassification
import torch

def py_ml_classifier(text, model_dir):
    model = BertForSequenceClassification.from_pretrained(model_dir, attn_implementation="eager")
    tokenizer = BertTokenizer.from_pretrained(model_dir)

    # Tokenize input text
    tokenized_input = tokenizer(text, padding=True, truncation=True, return_tensors="pt", max_length=512)

    # Ensure the model is in evaluation mode
    model.eval()

    # Make predictions
    with torch.no_grad():
        # Forward pass
        outputs = model(**tokenized_input)

    # Get the predicted probabilities
    probs = torch.nn.functional.softmax(outputs.logits, dim=-1)

    # Get the predicted class (0 or 1 in binary classification, 0 == No N-Value, 1 == N-Value)
    pred = torch.argmax(probs, dim=1)

    # Display results
    return { "classification": pred.numpy(), "prob": probs.numpy() }
