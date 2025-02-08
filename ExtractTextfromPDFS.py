import os
import PyPDF2

def extract_text_from_pdfs(directory, output_file):
    # Ouvrir le fichier texte en mode écriture
    with open(output_file, 'w', encoding='utf-8') as txt_file:
        # Parcourir tous les fichiers dans le répertoire
        for filename in os.listdir(directory):
            if filename.endswith('.pdf'):
                pdf_path = os.path.join(directory, filename)

                # Ouvrir le fichier PDF
                with open(pdf_path, 'rb') as pdf_file:
                    pdf_reader = PyPDF2.PdfReader(pdf_file)
                    total_pages = len(pdf_reader.pages)

                    # Parcourir chaque page du PDF
                    for page_num in range(total_pages):
                        page = pdf_reader.pages[page_num]
                        text = page.extract_text()

                        # Écrire le texte extrait dans le fichier texte
                        txt_file.write(text)
                        txt_file.write('\n')  # Ajouter une nouvelle ligne entre les textes de différentes pages


# Spécifiez le répertoire contenant les fichiers PDF et le fichier texte de sortie
directory = 'FR_ControlPDF'
output_file = 'FR_ControlPDF_text.txt'

extract_text_from_pdfs(directory, output_file)