-- Definition of data types
type Etudiant = String
type MontantADA = Double
type TransactionID = String
-- Function to simulate a payment transaction with error handling
enregistrerPaiement :: Etudiant -> MontantADA -> IO (Either String TransactionID)
enregistrerPaiement etudiant montant
  | montant <= 0 = return (Left "Le montant doit être positif.")
  | otherwise = do
      -- Simulation de l'enregistrement de la transaction dans une base de données ou un registre
      let transactionID = genererTransactionID etudiant montant
      putStrLn $ "Paiement de " ++ show montant ++ " ADA par " ++ etudiant ++ " enregistré. ID de transaction : " ++ transactionID
      return (Right transactionID)
-- Function to generate a unique transaction ID (simulation)
genererTransactionID :: Etudiant -> MontantADA -> TransactionID
genererTransactionID etudiant montant = "TXID-" ++ take 8 (show (hash (etudiant ++ show montant)))
  where
    hash str = foldl (\acc c -> 31 * acc + fromEnum c) 0 str
-- Main function to simulate a payment with modifiable data
main :: IO ()
main = do
  putStrLn "Bienvenue au système de paiement de l'ISDR-GL en ADA"
  putStrLn "Entrez le nom de l'étudiant (ou 'quitter' pour arrêter) :"
  etudiant <- getLine
  if etudiant == "quitter"
    then putStrLn "Opération annulée."
    else do
      putStrLn "Entrez le montant en ADA :"
      montantStr <- getLine
      case reads montantStr :: [(Double, String)] of
        [(montant, "")] -> do
          result <- enregistrerPaiement etudiant montant
          case result of
            Right transactionID -> putStrLn $ "Paiement effectué avec succès. ID de transaction : " ++ transactionID
            Left erreur -> putStrLn $ "Erreur : " ++ erreur
        _ -> putStrLn "Montant invalide. Veuillez entrer un nombre."
      main -- Appel récursif pour permettre d'autres paiements


